﻿namespace FShade

open System
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative


open System.Collections.Generic
open Aardvark.Base.ReflectionHelpers

type private PreprocessorState =
    {
        inputTopology : Option<InputTopology>
        vertexType : Type
        inputs : Dictionary<string, ParameterDescription>
        outputs : Dictionary<string, ParameterDescription>
        uniforms : Dictionary<string, UniformParameter>
        mutable builder : Expr
    }

    member x.AddInput(semantic : string, p : ParameterDescription) =
        match x.inputs.TryGetValue semantic with
            | (true, old) ->
                if old.paramType = p.paramType then ()
                else failwithf "[FShade] conflicting input-types for %s (%s vs %s)" semantic (getPrettyName old.paramType) (getPrettyName p.paramType)
            | _ ->
                x.inputs.[semantic] <- p


    member x.AddOutput(semantic : string, p : ParameterDescription) =
        match x.outputs.TryGetValue semantic with
            | (true, old) ->
                if old.paramType = p.paramType then ()
                else failwithf "[FShade] conflicting outputs-types for %s (%s vs %s)" semantic (getPrettyName old.paramType) (getPrettyName p.paramType)
            | _ ->
                x.outputs.[semantic] <- p
            
    member x.AddUniform( u : UniformParameter) =
        match x.uniforms.TryGetValue u.uniformName with
            | (true, old) ->
                if old.uniformType = u.uniformType then ()
                else failwithf "[FShade] conflicting uniform-types for %s (%s vs %s)" u.uniformName (getPrettyName old.uniformType) (getPrettyName u.uniformType)
            | _ ->
                x.uniforms.[u.uniformName] <- u

module private Preprocessor =
    open System.Reflection
    open System.Collections.Generic
    open Aardvark.Base.ReflectionHelpers

    [<AutoOpen>]
    module private Helpers = 
        let (|VertexLoad|_|) (vertexType : Type) (e : Expr) =
            match e with
                | PropertyGet(Some e, p, []) when e.Type = vertexType ->
                    let isRecordField = FSharpType.GetRecordFields(vertexType, true) |> Array.exists (fun pi -> pi = p)
                    if isRecordField then
                        let att = p.GetCustomAttributes<SemanticAttribute>(true) |> Seq.toList
                        match att with
                            | x::_ -> Some (e, x.Semantic, p)
                            | _ -> Some (e, p.Name, p)
                    else
                        failwith "[FShade] cannot access properties on vertex-input-type"
                | _ ->
                    None

        let (|Primitive|_|) (e : Option<Expr>) =
            match e with
                | Some e -> 
                    if e.Type.GetInterface("Primitive`1") <> null then
                        match e with
                            | Value _ | Var _ | PropertyGet(None, _, []) -> Some e
                            | _ -> None
                    else
                        None
                | None ->
                    None

    let private deconstructRecord (e : Expr) =
        match e with
            | NewRecord(t, args) -> 
                args

            | e ->
                let fields = FSharpType.GetRecordFields(e.Type, true)
                fields |> Array.toList |> List.map (fun f -> Expr.PropertyGet(e, f))

    let rec private writeOutput (state : PreprocessorState) (vertexIndices : Map<Var, Expr>) (vertex : Expr) =
        let fields = FSharpType.GetRecordFields(vertex.Type, true) |> Array.toList
        let values = deconstructRecord vertex |> List.map (preprocessInternal state vertexIndices)

        let writes =
            List.zip fields values
                |> List.map (fun (f, v) ->
                    let v = preprocessInternal state vertexIndices v
                    let sem = f.Semantic
                    match v with
                        | NewArray(et, args) ->
                            let value = Expr.NewFixedArray(et, args)
                            state.AddOutput(sem, { paramType = value.Type; paramInterpolation = InterpolationMode.Default })
                            sem, value
                                
                        | _ -> 
                            state.AddOutput(sem, { paramType = v.Type; paramInterpolation = InterpolationMode.Default })
                            (sem, v)
                )

        Expr.WriteOutputs writes

    and private preprocessInternal (state : PreprocessorState) (vertexIndices : Map<Var, Expr>) (e : Expr) =
        match e with
            | BuilderCall(b, mi, [Lambda(v,body)]) when mi.Name = "Delay" ->
                state.builder <- b
                preprocessInternal state vertexIndices body

            | BuilderCall(b, Method("Combine",_), [l;r]) ->
                state.builder <- b
                let l = preprocessInternal state vertexIndices l
                let r = preprocessInternal state vertexIndices r
                Expr.Seq [l; r]

            | BuilderCall(b, Method("Zero",_), []) ->
                state.builder <- b
                Expr.Unit

            // for v in primitive do
            | BuilderCall(b, Method("For",_) , [Coerce(primitive, t); Lambda(v,Let(vi,Var(vo),body))]) 
                when b.Type = typeof<GeometryBuilder> || b.Type = typeof<TessControlBuilder> || b.Type = typeof<TessEvalBuilder> ->
                state.builder <- b

                let count = primitive.Type.GetProperty("VertexCount", BindingFlags.Static ||| BindingFlags.Public).GetValue(null) |> unbox<int>
                
                let index = Var("i", typeof<int>)
                let vertexIndices = Map.add vi (Expr.Var index) vertexIndices
                let body = preprocessInternal state vertexIndices body
                Expr.ForIntegerRangeLoop(index, <@@ 0 @@>, <@@ count - 1 @@>,  body)

            | BuilderCall(b, Method("For",_), [seq; Lambda(v,Let(vi,Var(vo),body))]) ->  
                state.builder <- b   
                let body = preprocessInternal state vertexIndices body
                let seq = preprocessInternal state vertexIndices seq            
                let result = Expr.ForEach(vi, seq, body)
                result

            // let v = primitive.P0 || let v = primitive.VertexCount
            | Let(v, PropertyGet(Primitive p, pi, []), body) ->
                match pi.PrimitiveIndex with
                    | Some index -> 
                        let i = Var(v.Name, typeof<int>)
                        let vertexIndices = Map.add v (Expr.Var i) vertexIndices
                        
                        let body = preprocessInternal state vertexIndices body

                        Expr.Let(i, Expr.Value(index), body)

                    | _ ->               
                        let body = preprocessInternal state vertexIndices body
                        Expr.Let(v, Expr.PropertyGet(p, pi, []), body)

            // primitive.P0.pos || primitive.VertexCount.Abs
            | MemberFieldGet(PropertyGet(Primitive p, pi, []), m) ->
                match pi.PrimitiveIndex with
                    | Some index ->
                        let n = m.Semantic
                        state.AddInput(n, { paramType = e.Type.MakeArrayType(); paramInterpolation = InterpolationMode.Default })
                        Expr.ReadInput(ParameterKind.Input, e.Type, n, Expr.Value index)
                    | None ->
                        e

            // primitive.[i].pos
            | MemberFieldGet(PropertyGet(Some p, pi, [index]), m) when pi.Name = "Item" ->
                let sem = m.Semantic
                state.AddInput(sem, { paramType = e.Type.MakeArrayType(); paramInterpolation = pi.Interpolation })
                Expr.ReadInput(ParameterKind.Input, e.Type, sem, index)

            | VertexLoad state.vertexType (ve, sem, pi) ->
                match ve with
                    | Value(null,_) -> 
                        state.AddInput(sem, { paramType = e.Type; paramInterpolation = InterpolationMode.Default })
                        Expr.ReadInput(ParameterKind.Input, e.Type, sem)

                    | Var v ->
                        match Map.tryFind v vertexIndices with
                            | Some i -> 
                                state.AddInput(sem, { paramType = e.Type.MakeArrayType(); paramInterpolation = InterpolationMode.Default })
                                Expr.ReadInput(ParameterKind.Input, e.Type, sem, i)
                            | _ -> 
                                Expr.PropertyGet(ve, pi)
                                //failwithf "[FShade] accessing vertex input with unknown index"

                    | e ->
                        Expr.PropertyGet(ve, pi)
                        //failwithf "[FShade] complex vertex expressions not supported: %A" ve

            // yield (let a = x+y in f(a))   ==>    let a = x + y in yield f(a)
            | BuilderCall(b, mi, [Let(v, e, inner)]) when mi.Name = "Yield" ->
                state.builder <- b
                Expr.Let(v, e, Expr.Call(b, mi, [inner])) |> preprocessInternal state vertexIndices


            // yield { a = 1; b = x + y }
            | BuilderCall(b, mi, [vertex]) when mi.Name = "Yield" && FSharpType.IsRecord vertex.Type ->
                state.builder <- b
                Expr.Seq [
                    writeOutput state vertexIndices vertex
                    <@ emitVertex() @>
                ]

            // yield V4d.IOOI
            | BuilderCall(b, mi, [value]) when mi.Name = "Yield" ->
                state.builder <- b
                let value = preprocessInternal state vertexIndices value
                state.AddOutput(Intrinsics.Position, { paramType = typeof<V4d>; paramInterpolation = InterpolationMode.Default })
                let store = 
                    if value.Type = typeof<V4d> then 
                        Expr.WriteOutputs [Intrinsics.Position, value]

                    elif value.Type = typeof<V3d> then 
                        let ctor = typeof<V4d>.GetConstructor [|typeof<V3d>; typeof<float> |]
                        Expr.WriteOutputs [Intrinsics.Position, Expr.NewObject(ctor, [value; Expr.Value 1.0])]

                    else 
                        failwithf "[FShade] cannot yield %A" value
                    
                Expr.Sequential(store, Expr.Call(getMethodInfo <@ emitVertex @>, []))

            // return (let a = x+y in f(a))   ==>    let a = x + y in return f(a)
            | BuilderCall(b, mi, [Let(v, e, inner)]) when mi.Name = "Return" ->
                state.builder <- b
                Expr.Let(v, e, Expr.Call(b, mi, [inner])) |> preprocessInternal state vertexIndices

            | BuilderCall(b, mi, [vertex]) when mi.Name = "Return" && FSharpType.IsRecord vertex.Type ->
                state.builder <- b
                writeOutput state vertexIndices vertex

            // return V4d.IOOI
            | BuilderCall(b, mi, [value]) when mi.Name = "Return" ->
                state.builder <- b
                let value = preprocessInternal state vertexIndices value
                let sem = 
                    if b.Type = typeof<FragmentBuilder> then Intrinsics.Color
                    else Intrinsics.Position
                        
                state.AddOutput(sem, { paramType = typeof<V4d>; paramInterpolation = InterpolationMode.Default })
                if value.Type = typeof<V4d> then
                    Expr.WriteOutputs [sem, value]
                elif value.Type = typeof<V3d> then
                    let ctor = typeof<V4d>.GetConstructor [|typeof<V3d>; typeof<float> |]
                    Expr.WriteOutputs [sem, Expr.NewObject(ctor, [value; Expr.Value 1.0])]
                else
                    failwithf "[FShade] cannot return %A" value
                       
                
            | Uniform u ->
                state.AddUniform u
                Expr.ReadInput(ParameterKind.Uniform, u.uniformType, u.uniformName)
                       
                        
            | ShapeCombination(o, args) ->
                let args = args |> List.map (preprocessInternal state vertexIndices)
                RebuildShapeCombination(o, args)

            | ShapeLambda(v,b) -> 
                Expr.Lambda(v, preprocessInternal state vertexIndices b)

            | ShapeVar _ -> 
                e


    /// preprocess removes all builder calls from the given expression, 
    /// replaces all input-accesses with Expr.Load and all writes with Expr.Store.
    /// additionaly the used builder 
    let preprocessShader (inputType : Type) (e : Expr) =

        // figure out the vertex-type and the (optional) input-topology
        let vertexType, inputTopology =
            let p = inputType.GetInterface("Primitive`1")
            if isNull p then inputType, None
            else 
                let vertexType = p.GetGenericArguments().[0]
                let top = inputType.GetProperty("InputTopology", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public).GetValue(null) |> unbox<InputTopology>
                vertexType, Some top

        // the builder can be inlined (TODO: may be nested)
        let input = 
            match e with
                | Application(Lambda(var, b), value) -> b.Substitute(fun vi -> if vi = var then Some value else None)
                | e -> e

        // run the real processing
        let state = 
            {
                inputTopology = inputTopology
                vertexType = vertexType
                inputs = Dictionary()
                outputs = Dictionary()
                uniforms = Dictionary()
                builder = Expr.Value(())
            }

        let clean = input |> preprocessInternal state Map.empty


        clean, state

    let preprocess (e : Expr) =
        // run the real processing
        let state = 
            {
                inputTopology = None
                vertexType = typeof<obj>
                inputs = Dictionary()
                outputs = Dictionary()
                uniforms = Dictionary()
                builder = Expr.Value(())
            }

        let clean = e |> preprocessInternal state Map.empty
        clean

    let usedInputs (e : Expr) =
        let rec visit (inputs : Dictionary<string, ParameterKind * Type>) (e : Expr) =
            match e with
                | ReadInput(kind, name, idx) ->
                    idx |> Option.iter (visit inputs)
                    inputs.[name] <- (kind, e.Type)

                | ShapeVar v ->
                    ()

                | ShapeLambda(v,b) ->
                    visit inputs b

                | ShapeCombination(o, args) ->
                    for a in args do
                        visit inputs a

        let inputs = Dictionary<string, ParameterKind * Type>()
        visit inputs e
        inputs
        


    
type ShaderOutputValue(mode : InterpolationMode, value : Expr) =
    let value, state = Preprocessor.preprocessShader typeof<obj> value
    do 
        let inputs = Preprocessor.usedInputs value
        for (n,(kind, t)) in Dictionary.toSeq inputs do
            if kind = ParameterKind.Input && not (state.inputs.ContainsKey n) then
                state.inputs.[n] <- { paramType = t; paramInterpolation = InterpolationMode.Default }

    member x.Type = value.Type
    member x.Value = value
    member x.Interpolation = mode
    member x.UsedInputs = state.inputs
    member x.UsedUniforms = state.uniforms

    new(value : Expr) = ShaderOutputValue(InterpolationMode.Default, value)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ShaderOutputValue =
    let inline value (o : ShaderOutputValue) = o.Value
    let inline interpolation (o : ShaderOutputValue) = o.Interpolation
    let inline outputType (o : ShaderOutputValue) = o.Type
    let inline usedInputs (o : ShaderOutputValue) = o.UsedInputs
    let inline usedUniforms (o : ShaderOutputValue) = o.UsedUniforms

    let toParameterDescription (o : ShaderOutputValue) =
        { paramType = o.Type; paramInterpolation = o.Interpolation }

    let ofParameterDescription (p : ParameterDescription) (value : Expr) =
        ShaderOutputValue(p.paramInterpolation, value)

type Shader =
    {
        shaderStage             : ShaderStage
        shaderInputs            : Map<string, ParameterDescription>
        shaderOutputs           : Map<string, ParameterDescription>
        shaderUniforms          : Map<string, UniformParameter>
        shaderInputTopology     : Option<InputTopology>
        shaderOutputTopology    : Option<OutputTopology * int>
        shaderBody              : Expr
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Shader =

    let private builtInInputs =
        Dictionary.ofList [
            ShaderStage.Vertex,
                Map.ofList [
                    Intrinsics.VertexId, typeof<int>
                    Intrinsics.InstanceId, typeof<int>
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.PatchVertices, typeof<int>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.InvocationId, typeof<int>
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.TessCoord, typeof<V3d>
                    Intrinsics.PatchVertices, typeof<int>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.TessLevelInner, typeof<Arr<2 N, float>>
                    Intrinsics.TessLevelOuter, typeof<Arr<4 N, float>>
                ]
                
            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.InvocationId, typeof<int>
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.FragCoord, typeof<V4d>
                    Intrinsics.PointCoord, typeof<V2d>
                    Intrinsics.FrontFacing, typeof<bool>
                    Intrinsics.SampleId, typeof<int>
                    Intrinsics.SamplePosition, typeof<V2d>
                    Intrinsics.SampleMask, typeof<int[]>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.PrimitiveId, typeof<int>
                    Intrinsics.Layer, typeof<int>
                    Intrinsics.ViewportIndex, typeof<int>
                ]

        ]

    let private builtInOutputs =
        Dictionary.ofList [
            ShaderStage.Vertex,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                ]

            ShaderStage.TessControl,
                Map.ofList [
                    Intrinsics.TessLevelInner, typeof<Arr<2 N, float>>
                    Intrinsics.TessLevelOuter, typeof<Arr<4 N, float>>
                ]

            ShaderStage.TessEval,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                ]

            ShaderStage.Geometry,
                Map.ofList [
                    Intrinsics.PointSize, typeof<float>
                    Intrinsics.ClipDistance, typeof<float[]>
                    Intrinsics.Layer, typeof<int>
                    Intrinsics.ViewportIndex, typeof<int>
                    Intrinsics.SourceVertexIndex, typeof<int>
                ]

            ShaderStage.Fragment,
                Map.ofList [
                    Intrinsics.Depth, typeof<float>
                    Intrinsics.SampleMask, typeof<float[]>
                ]

        ]

    let private sideEffects =
        Dictionary.ofList [
            ShaderStage.Vertex, 
                HashSet.empty

            ShaderStage.TessControl, 
                HashSet.empty

            ShaderStage.TessEval, 
                HashSet.empty

            ShaderStage.Geometry, 
                HashSet.ofList [
                    getMethodInfo <@ emitVertex @>
                    getMethodInfo <@ restartStrip @>
                    getMethodInfo <@ endPrimitive @>
                ]

            ShaderStage.Fragment, 
                HashSet.ofList [
                    getMethodInfo <@ discard @>
                ]
        ]

    let private converter (inType : Type) (outType : Type) : Expr -> Expr =
        if inType <> outType then
            failwithf "[FShade] cannot convert value from %A to %A" inType outType
        else
            id
        
    let private tryGetSourceVertexIndex (shader : Shader) (values : Map<string, Expr>) =
        match shader.shaderInputTopology with
            | Some InputTopology.Point -> Expr.Value 0 |> Some
            | _ -> Map.tryFind Intrinsics.SourceVertexIndex values

    let private tryGetSourceVertexIndexValue (shader : Shader) (values : Map<string, ShaderOutputValue>) =
        match shader.shaderInputTopology with
            | Some InputTopology.Point -> ShaderOutputValue(Expr.Value 0) |> Some
            | _ -> Map.tryFind Intrinsics.SourceVertexIndex values

    let private emitVertexMeth = getMethodInfo <@ emitVertex @>

    let systemInputs (shader : Shader) =
        let builtIn = builtInInputs.[shader.shaderStage]
        shader.shaderInputs |> Map.choose (fun name p ->
            match Map.tryFind name builtIn with
                | Some neededType ->
                    let typesValid =
                        match neededType, p.paramType with
                            | (ArrOf(_, nt) | ArrayOf nt), (ArrOf(_, pt) | ArrayOf pt) -> nt = pt
                            | n, p -> n = p

                    if not typesValid then
                        failwithf "[FShade] invalid system-input type %A (should be %A)" p.paramType neededType

                    Some p.paramType
                | None ->
                    None
        )

    let systemOutputs (shader : Shader) =
        let builtIn = builtInOutputs.[shader.shaderStage]
        shader.shaderOutputs |> Map.choose (fun name p ->
            match Map.tryFind name builtIn with
                | Some neededType ->
                    let typesValid =
                        match neededType, p.paramType with
                            | (ArrOf(_, nt) | ArrayOf nt), (ArrOf(_, pt) | ArrayOf pt) -> nt = pt
                            | n, p -> n = p

                    if not typesValid then
                        failwithf "[FShade] invalid system-output type %A (should be %A)" p.paramType neededType

                    Some p.paramType
                | None ->
                    None
        )

    let inline stage (s : Shader) = s.shaderStage
    let inline uniforms (s : Shader) = s.shaderUniforms
    let inline body (s : Shader) = s.shaderBody
    let inline inputTopology (s : Shader) = s.shaderInputTopology
    let inline outputTopology (s : Shader) = s.shaderOutputTopology

    /// optimizes the shader by
    ///    1) evaluating constant expressions
    ///    2) removing useless expressions/variables
    ///    3) unrolling loops where annotated
    ///
    /// and might in the future:
    ///    3) inline copy variables
    ///    4) inline functions where possible
    let optimize (shader : Shader) =
        let sideEffects = sideEffects.[shader.shaderStage]
        let newBody = 
            shader.shaderBody
                |> Optimizer.evaluateConstants' sideEffects.Contains
                |> Optimizer.eliminateDeadCode' sideEffects.Contains

        let inputs = Preprocessor.usedInputs newBody

        let newOutputTopology =
            match shader.shaderStage with
                | ShaderStage.Geometry -> 
                    let top, _ = Option.get shader.shaderOutputTopology
                    let range = Expr.computeCallCount emitVertexMeth newBody
                    let maxVertices =
                        if range.Max = Int32.MaxValue then
                            Log.warn "[FShade] could not determine max-vertex-count (using 32)"
                            32
                        else
                            range.Max
                    Some (top, range.Max)
                | _ ->
                    None

        { shader with
            shaderInputs = shader.shaderInputs |> Map.filter (fun n _ -> inputs.ContainsKey n)
            shaderUniforms = shader.shaderUniforms |> Map.filter (fun n _ -> inputs.ContainsKey n)
            shaderOutputTopology = newOutputTopology
            shaderBody = newBody
        }

    /// creates a shader using the given vertex-input-type and body
    let ofExpr (vertexType : Type) (body : Expr) =
        let body, state = Preprocessor.preprocessShader vertexType body

        // figure out the used builder-type
        let builder = 
            match Expr.TryEval state.builder with
                | Some (:? IShaderBuilder as v) -> v
                | _ -> failwithf "[FShade] could not evaluate shader-builder %A" state.builder

        optimize { 
            shaderStage             = builder.ShaderStage
            shaderInputs            = state.inputs |> Dictionary.toMap
            shaderOutputs           = state.outputs |> Dictionary.toMap
            shaderUniforms          = state.uniforms |> Dictionary.toMap
            shaderInputTopology     = state.inputTopology
            shaderOutputTopology    = builder.OutputTopology |> Option.map (fun t -> t, Int32.MaxValue)
            shaderBody              = body
        }

    /// creates a shader using the given function
    let ofFunction (shaderFunction : 'a -> Expr<'b>) =
        let expression = 
            try shaderFunction Unchecked.defaultof<'a>
            with _ -> failwith "[FShade] shader functions may not access their vertex-input statically"
        ofExpr typeof<'a> expression


    /// creates a new shader by modifying all output-writes.
    /// this can be used for adding/removing outputs for shaders
    let mapOutputs (f : Map<string, ShaderOutputValue> -> Map<string, ShaderOutputValue>) (shader : Shader) =
        let mutable newOutputs = Map.empty
        let mutable newInputs = shader.shaderInputs
        let mutable newUniforms = shader.shaderUniforms

        let addValue (name : string) (value : ShaderOutputValue) =
            newOutputs <- Map.add name (ShaderOutputValue.toParameterDescription value) newOutputs

            for (n, pd) in Dictionary.toSeq value.UsedInputs do
                let inputParameter = 
                    match Map.tryFind n newInputs with
                        | Some i -> i
                        | None -> pd

                newInputs <- Map.add name inputParameter newInputs

            for (n, up) in Dictionary.toSeq value.UsedUniforms do
                let uniformParameter =
                    match Map.tryFind n newUniforms with
                        | Some u -> u
                        | None -> up

                newUniforms <- Map.add name uniformParameter newUniforms

            value.Value

        let newBody =
            shader.shaderBody.SubstituteWrites (fun values ->
                values
                    |> Map.map (fun n -> ShaderOutputValue.ofParameterDescription shader.shaderOutputs.[n])
                    |> f
                    |> Map.map addValue
                    |> Expr.WriteOutputs
                    |> Some
            )

        let newShader =
            { shader with
                shaderUniforms          = newUniforms
                shaderInputs            = newInputs
                shaderOutputs           = newOutputs
                shaderBody              = newBody
            }

        let outputsRemoved = 
            shader.shaderOutputs |> Map.keys |> Seq.exists (fun n -> not (Map.containsKey n newOutputs))

        optimize newShader

    /// creates a new shader having exactly the the given outputs by:
    ///     1) removing the ones that are not needed
    ///     2) adding the ones that are not existing
    ///     3) changing the type (using value-conversions) for the ones with bad types
    /// NOTE: by passing typeof<obj> for a semantic the function does not 
    ///       change existing types but can obviously not add a new output for it.
    let withOutputs (outputs : Map<string, Type>) (shader : Shader) =
        let current = shader.shaderOutputs |> Map.map (fun n p -> p.paramType)

        if current = outputs then
            shader
        else
            shader |> mapOutputs (fun values ->
                outputs 
                    |> Map.map (fun n t ->
                        match Map.tryFind n values with
                            | Some value ->
                                if not (t.IsAssignableFrom value.Type) then
                                    failwithf "[FShade] cannot change output-type for %s from %A to %A" n value.Type t
                                
                                value
                            | None ->
                                if t = typeof<obj> then
                                    failwithf "[FShade] cannot add output %A with object type" n
                                    
                                match shader.shaderStage with
                                    | ShaderStage.Vertex | ShaderStage.Fragment ->
                                        ShaderOutputValue(Expr.ReadInput(ParameterKind.Input, t, n))

                                    | ShaderStage.Geometry ->
                                        match tryGetSourceVertexIndexValue shader values with
                                            | Some index -> 
                                                ShaderOutputValue(Expr.ReadInput(ParameterKind.Input, t, n, index.Value))
                                            | None -> 
                                                failwithf "[FShade] cannot add output %A to GeometryShader since no SourceVertexIndex was available" n
                                    
                                    | _ ->
                                        failwith "[FShade] passing for tessellation not implemented"
                                                
                    )
            )

    let removeOutputs (semantics : Set<string>) (shader : Shader) =
        let desired = 
            shader.shaderOutputs |> Map.choose (fun name p ->
                if Set.contains name semantics then None
                else Some p.paramType
            )

        withOutputs desired shader

    /// translates a shader to an EntryPoint which can be used for compiling
    /// the shader to a CAst
    let toEntryPoint (prev : Option<ShaderStage>) (s : Shader) (next : Option<ShaderStage>) =
        let inputs = 
            s.shaderInputs |> Map.toList |> List.map (fun (n,i) -> 
                { 
                    paramName = n
                    paramSemantic = n
                    paramType = i.paramType
                    paramDecorations = Set.ofList [ParameterDecoration.Interpolation i.paramInterpolation]
                }
            )

        let outputs = 
            s.shaderOutputs |> Map.toList |> List.map (fun (n, i) -> 
                { 
                    paramName = n
                    paramSemantic = n
                    paramType = i.paramType
                    paramDecorations = Set.empty
                }
            )

        let uniforms =
            s.shaderUniforms |> Map.toList |> List.map (fun (n, u) ->
                let uniformBuffer = 
                    match u.uniformValue with
                        | Attribute(scope, name) -> Some scope.FullName
                        | _ -> None
                { 
                    uniformName = u.uniformName
                    uniformType = u.uniformType
                    uniformBuffer = uniformBuffer
                }
            )

        {
            conditional = s.shaderStage |> string |> Some
            entryName   = "main"
            inputs      = inputs
            outputs     = outputs
            uniforms    = uniforms
            arguments   = []
            body        = s.shaderBody
            decorations = 
                List.concat [
                    [ EntryDecoration.Stages { prev = prev; self = s.shaderStage; next = next } ]
                    s.shaderInputTopology |> Option.map EntryDecoration.InputTopology |> Option.toList
                    s.shaderOutputTopology |> Option.map EntryDecoration.OutputTopology |> Option.toList
                ]
        }

    /// creates a shader "passing-thru" all supplied attributes
    let passing (attributes : Map<string, Type>) (stage : ShaderStage) =
        match stage with
            | ShaderStage.Vertex | ShaderStage.Fragment ->
                let parameters = attributes |> Map.map (fun _ -> ParameterDescription.ofType)
                {
                    shaderStage             = stage
                    shaderInputs            = parameters
                    shaderOutputs           = parameters
                    shaderUniforms          = Map.empty
                    shaderInputTopology     = None
                    shaderOutputTopology    = None
                    shaderBody =
                        attributes
                            |> Map.map (fun n t -> Expr.ReadInput(ParameterKind.Input, t, n))
                            |> Expr.WriteOutputs
            
                }
            | _ ->
                failwith "[FShade] not implemented"
    
    /// creates a shader (for the given stage) with no in-/outputs
    let empty (stage : ShaderStage) = 
        passing Map.empty stage

    let inputs (shader : Shader) =
        match shader.shaderStage with
            | ShaderStage.Fragment ->
                shader.shaderInputs
                    |> Map.map (fun _ p -> p.paramType)
                    |> Map.add Intrinsics.Position typeof<V4d>

            | ShaderStage.Geometry ->
                shader.shaderInputs
                    |> Map.map (fun _ p ->
                        match p.paramType with
                            | ArrOf(_,t) | ArrayOf t -> t
                            | t -> t
                    )

            | _ ->
                shader.shaderInputs
                    |> Map.map (fun _ p -> p.paramType)
                
    let outputs (shader : Shader) =
        match shader.shaderStage with
            | ShaderStage.Fragment ->
                shader.shaderOutputs
                    |> Map.map (fun _ p -> p.paramType)
                    |> Map.add Intrinsics.Depth typeof<float>
            | _ ->
                shader.shaderOutputs
                    |> Map.map (fun _ p -> p.paramType)
        

    module private Composition = 
        let simple (l : Shader) (r : Shader) =
            let needed  = Map.intersect l.shaderOutputs r.shaderInputs
            let passed  = Map.difference l.shaderOutputs r.shaderOutputs

            let lBody =
                l.shaderBody.SubstituteWrites (fun values ->
                    let variables =
                        needed |> Map.map (fun name (lv, rv) -> 
                            let variable = Var(name + "C", rv.paramType)
                            let converter = converter lv.paramType rv.paramType
                            variable, converter
                        )

                    let rBody =
                        r.shaderBody
                            |> Expr.substituteReads (fun kind t name idx ->
                                match kind with
                                    | ParameterKind.Input -> 
                                        match Map.tryFind name variables with
                                            | Some(v,_) -> Expr.Var v |> Some
                                            | _ -> None
                                    | _ ->
                                        None
                            )

                    let rBody = 
                        variables |> Map.fold (fun b name (var, convert) ->
                            Expr.Let(var, convert (Map.find name values), b)
                        ) rBody

                    let passedValues =
                        passed |> Map.map (fun name p ->
                            Map.find name values
                        )

                    if Map.isEmpty passedValues then
                        rBody |> Some
                    else
                        rBody
                            |> Expr.substituteWrites (fun rValues ->
                                Map.union passedValues rValues
                                    |> Expr.WriteOutputs
                                    |> Some
                            )
                            |> Some
                )

            optimize 
                { l with
                    shaderInputs = Map.union r.shaderInputs l.shaderInputs
                    shaderOutputs = Map.union l.shaderOutputs r.shaderOutputs
                    shaderUniforms = Map.union l.shaderUniforms r.shaderUniforms
                    shaderBody = lBody
                }

        let gsvs (lShader : Shader) (rShader : Shader) =
            let needed  = Map.intersect lShader.shaderOutputs rShader.shaderInputs
            let passed  = Map.difference lShader.shaderOutputs rShader.shaderOutputs
            let unknown = Map.difference rShader.shaderInputs lShader.shaderOutputs |> Map.keys |> Set.ofSeq
            let mutable finalOutputsFromRight = rShader.shaderOutputs |> Map.keys |> Set.ofSeq
            
            let lBody =
                lShader.shaderBody.SubstituteWrites (fun values ->
                    let variables =
                        needed |> Map.map (fun name (lv, rv) -> 
                            let variable = Var(name + "C", rv.paramType)
                            let converter = converter lv.paramType rv.paramType
                            variable, converter
                        )

                    let vertexIndex = tryGetSourceVertexIndex lShader values

                    let rShader =
                        match tryGetSourceVertexIndex lShader values with
                            | Some _ -> rShader
                            | None when Set.isEmpty unknown -> rShader
                            | None ->
                                let invalidOutputs = 
                                    let affected = Expr.getAffectedOutputsMap rShader.shaderBody
                                    unknown 
                                        |> Seq.map (fun u -> match Map.tryFind u affected with | Some a -> a | _ -> Set.empty) 
                                        |> Set.unionMany

                                finalOutputsFromRight <- Set.difference finalOutputsFromRight invalidOutputs

                                rShader |> removeOutputs invalidOutputs
                            
                    let rBody = 
                        rShader.shaderBody
                            |> Expr.substituteReads (fun kind t name idx ->
                                match kind with
                                    | ParameterKind.Input -> 
                                        match Map.tryFind name variables with
                                            | Some(v,_) -> 
                                                Expr.Var v |> Some
                                            | None -> 
                                                match vertexIndex with
                                                    | Some vertexIndex -> 
                                                        match idx with
                                                            | None -> Some (Expr.ReadInput(kind, t, name, vertexIndex))
                                                            | Some i -> failwithf "[FShade] vertex shader reading indexed input %A not supported" name
                                                    | None ->
                                                        failwithf "[FShade] internal error in gsvs"
                                    | _ -> 
                                        None
                            )
                             
                    let rBody = 
                        variables |> Map.fold (fun b name (var, convert) ->
                            Expr.Let(var, convert (Map.find name values), b)
                        ) rBody

                    let passedValues =
                        passed |> Map.map (fun name p ->
                            Map.find name values
                        )

                    if Map.isEmpty passedValues then
                        rBody |> Some
                    else
                        rBody
                            |> Expr.substituteWrites (fun rValues ->
                                Map.union passedValues rValues
                                    |> Expr.WriteOutputs
                                    |> Some
                            )
                            |> Some
                )

            let rInputs = 
                rShader.shaderInputs |> Map.choose (fun name p ->
                    if Set.contains name unknown then
                        Some { p with paramType = p.paramType.MakeArrayType() }
                    else
                        None
                )

            let rOutputs =
                rShader.shaderOutputs |> Map.filter (fun name p -> Set.contains name finalOutputsFromRight)

            optimize 
                { lShader with
                    shaderInputs = Map.union rInputs lShader.shaderInputs
                    shaderOutputs = Map.union lShader.shaderOutputs rOutputs
                    shaderUniforms = Map.union lShader.shaderUniforms rShader.shaderUniforms
                    shaderBody = lBody
                }

    let compose2 (l : Shader) (r : Shader) =
        match l.shaderStage, r.shaderStage with

            // simple case: both are vertex/fragment
            | ShaderStage.Vertex, ShaderStage.Vertex
            | ShaderStage.Fragment, ShaderStage.Fragment ->
                Composition.simple l r

            // harder case: left is geometry and right is vertex
            | ShaderStage.Geometry, ShaderStage.Vertex ->
                Composition.gsvs l r


            | _ ->
                failwithf "[FShade] cannot compose %AShader with %AShader" l.shaderStage r.shaderStage

    let compose (l : #seq<Shader>) =
        use e = l.GetEnumerator()
        if e.MoveNext() then
            let mutable res = e.Current
            while e.MoveNext() do
                res <- compose2 res e.Current
            res
        else
            failwith "[FShade] cannot compose empty shader-sequence"      
