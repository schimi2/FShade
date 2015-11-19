namespace FShade

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection
open Aardvark.Base
open FShade.Utils
open FShade.Compiler

type FShadeEffect = Error<Effect> 

[<AutoOpen>]
module EffectCompilation =

    let toShader (inputTopology : Option<InputTopology>) (e : Expr) =
        transform {
            match e with
                | MethodLambda(args, body) -> 
                    do! resetCompilerState

                    //eliminate useless builder calls and get the shaderType
                    let! body = removeBuilderCalls body

                    //replace all uniforms with variables and return a map
                    let! body = substituteUniforms body
                    let! state0 = compilerState
                    let uniforms0  = state0.uniforms |> HashMap.toList


                    //replace all input-accesses with variables and return a map
                    let a = List.head args
                    let! body = substituteInputs a.Type None body
                
                    //replace all outputs with variables and return a map 
                    let outputType = body.Type

                    let! body = substituteOutputs outputType body

                    let! state = compilerState

                    let uniforms  = uniforms0

                    let builder = match state.builder with
                                        | Some b -> 
                                            match Expr.tryEval b with
                                                | Some b -> b|> unbox<IShaderBuilder>
                                                | None -> failwith "could not evaluate IShaderBuilder"
                                        | None -> failwith ""


                    return { shaderType = builder.ShaderType; uniforms = uniforms; inputs = state.inputs; outputs = state.outputs; body = body; inputTopology = inputTopology; debugInfo = None }

                | _ -> return! error "not a function"
        }

    let createPassingShader (prim : Type) (id : Var)=
        compile {
            let vertexType = prim.GetGenericArguments().[0]
            let fields = FSharpTypeExt.GetRecordFields(vertexType)

            let input = Var("input", prim)
//            let item = prim.Type.GetProperty("Item")
//            let inputAtIndex = Expr.PropertyGet(Expr.Var input, item, [Expr.Var id])

            let! fields = fields |> Array.toList |> List.mapC (fun m -> 
                            compile {
                                let item = prim.GetProperty("Item")

                                let v = Expr.PropertyGet(Expr.Var input, item, [Expr.Var id])
                                return Expr.PropertyGet(v, m)
                            } )



            let ret = typeof<VertexBuilder>.GetMethod("Return")
            let ret = ret.MakeGenericMethod [|vertexType|]
            let value = Expr.NewRecord(vertexType, fields)
            let shaderCode = Expr.Call(Expr.Value(vertex), ret, [value])

            let shaderCode = Expr.Lambda(input, shaderCode)
            let top = prim.GetProperty("InputTopology").GetValue(null) |> unbox<InputTopology> |> Some

            
            return! toShader top shaderCode
        }

    

    let toShader' (f : 'a -> Expr<'b>) =
        
        let outputType = typeof<'b>

        let e = Unchecked.defaultof<'a> |> f
        let debugInfo = ShaderDebug.tryGetShaderInfo f e
        let e = Expr.Lambda(Var("input", typeof<'a>), e)

        let prim = typeof<'a>.GetInterface("Primitive`1")
        let itop = 
            if prim = null then 
                None 
            else 
                typeof<'a>.GetProperty("InputTopology").GetValue(null) |> unbox<InputTopology> |> Some

        compile {
            let! s = toShader itop e

            if s.shaderType = ShaderType.TessControl then

                let _,inner = s.outputs.["TessLevelInner"]
                let _,outer = s.outputs.["TessLevelOuter"]

                let rec padTessLevels (e : Expr) =
                    match e with
                        | VarSet(v, NewArray(t,values)) when v = inner || v = outer ->
                            let count = if v = inner then 2 else 4
                            
                            let argCount = values.Length
                            if argCount < count then
                                let values = List.concat [values; List.init (count - argCount) (fun _ -> Expr.Value(1.0))]
                                Expr.VarSet(v, Expr.NewArray(t, values))
                            elif argCount > count then
                                let values = values |> Seq.take count |> Seq.toList
                                Expr.VarSet(v, Expr.NewArray(t, values))
                            else
                                e

                        | ShapeLambda(v, b) -> Expr.Lambda(v, padTessLevels b)
                        | ShapeCombination(o, args) -> RebuildShapeCombination(o, args |> List.map padTessLevels)
                        | _ -> e

                let id = Var("InvocationId", typeof<int>)
                let mi = getMethodInfo <@ (=) : int -> int -> bool @>
                let mi = mi.MakeGenericMethod [| typeof<int> |]

                let newBody = Expr.IfThenElse(Expr.Call(mi, [Expr.Var id; Expr.Value(0)]),
                                padTessLevels s.body,
                                Expr.Value(())
                               )

                return [{ s with body = newBody; 
                                 inputs = Map.add "InvocationId" id s.inputs
                                 debugInfo = debugInfo }]
            else
                return [{ s with debugInfo = debugInfo }]
        }

    let private toEffectInternalCache = 
        MemoCache (fun (s : Shader) ->
            match s.shaderType with
                | Vertex -> { vertexShader = Some s; geometryShader = None; tessControlShader = None; tessEvalShader = None; fragmentShader = None; originals = [s] }
                | Fragment -> { vertexShader = None; geometryShader = None; tessControlShader = None; tessEvalShader = None; fragmentShader = Some s; originals = [s] }
                | Geometry(maxVertices, t) -> { vertexShader = None; geometryShader = Some(s,t); tessControlShader = None; tessEvalShader = None; fragmentShader = None; originals = [s] } 
                | TessControl -> { vertexShader = None; geometryShader = None; tessControlShader = Some s; tessEvalShader = None; fragmentShader = None; originals = [s] }
                | TessEval -> { vertexShader = None; geometryShader = None; tessControlShader = None; tessEvalShader = Some s; fragmentShader = None; originals = [s] }
        )

    let toEffectInternal (s : Shader) =
        toEffectInternalCache.Invoke s

    let private translateState =
        { compiler = Unchecked.defaultof<_>
          types = PersistentHashSet.empty
          functions = PersistentHashSet.empty
          constantId = 0
          constants = HashMap.empty
          
          lambdaId = 0
          lambdas = Map.empty
          
          defines = Map.empty
          functionId = 0
          
          uniformId = 0
          uniforms = HashMap.empty
          
          bound = Set.empty
          userState = ShaderState.emptyShaderState 
          initialUserState = ShaderState.emptyShaderState  
        }

    
    let private createEffect (f : 'a -> Expr<'b>) =
        let compiled =
            transform {
                let! shaders = toShader' f
                let mutable result = { vertexShader = None; geometryShader = None; tessControlShader = None; tessEvalShader = None; fragmentShader = None; originals = shaders }
                do for s in shaders do
                    match s.shaderType with
                        | Vertex -> result <- { result with vertexShader = Some s }
                        | Fragment ->  result <- { result with fragmentShader = Some s }
                        | Geometry(maxVertices, t) ->  result <- { result with geometryShader = Some(s, t) } 
                        | TessControl ->  result <- { result with tessControlShader = Some s }
                        | TessEval ->  result <- { result with tessEvalShader = Some s }
                return result
            }

        match compiled.runCompile translateState with
            | Success (_,v) -> Success v
            | Error e -> Error e



    let private tryReplaceUserUniformsInShader (oldClosure : Reflection.Closure) (newClosure : Reflection.Closure) (s : Shader) =
        
        let userUniformValues =
            s.uniforms 
                |> List.choose (fun (u,_) ->
                    match u with
                        | UserUniform(t,v) -> Some v
                        | _ -> None
                )
                |> PersistentHashSet.ofList
        
        let oldConstants =
            oldClosure.args
                |> List.mapi (fun i l ->
                    match l with
                        | Reflection.Constant l when PersistentHashSet.contains l userUniformValues -> Some(i,l)
                        | _ -> None
                   )
                |> List.choose id

        let allDistinct = System.Collections.Generic.HashSet(List.map snd oldConstants).Count = List.length oldConstants

        if allDistinct then
            let success = ref true

            let constants =
                oldConstants |> List.choose (fun (i,l) ->
                    match newClosure.args.[i] with
                        | Reflection.Constant r -> Some (l,r)
                        | _ -> None
                )

            let table = constants |> HashMap.ofList

            let newUniforms =
                s.uniforms |> List.map (fun (u,var) ->
                    match u with
                        | UserUniform(t,value) -> 
                            match HashMap.tryFind value table with
                                | Some n -> UserUniform(t, n), var
                                | None -> success := false; u,var //failwithf "could not get user-uniform %A from closure" var
                        | _ -> u,var
                )
            if !success then Some { s with uniforms = newUniforms}
            else None
        else
            None

    let private tryReplaceUserUniforms (oldClosure : Reflection.Closure) (newClosure : Reflection.Closure) (e : Effect) =
        Aardvark.Base.Monads.Option.option {
            let! vs =
                match e.vertexShader with
                    | Some vs -> tryReplaceUserUniformsInShader oldClosure newClosure vs |> Option.map Some
                    | None -> Some None
            let! tcs =
                match e.tessControlShader with
                    | Some vs -> tryReplaceUserUniformsInShader oldClosure newClosure vs |> Option.map Some
                    | None -> Some None
            let! tev =
                match e.tessEvalShader with
                    | Some vs -> tryReplaceUserUniformsInShader oldClosure newClosure vs |> Option.map Some
                    | None -> Some None
            let! gs =
                match e.geometryShader with
                    | Some (gs,top) -> tryReplaceUserUniformsInShader oldClosure newClosure gs |> Option.map (fun gs -> Some(gs, top))
                    | None -> Some None
            let! fs =
                match e.fragmentShader with
                    | Some vs -> tryReplaceUserUniformsInShader oldClosure newClosure vs |> Option.map Some
                    | None -> Some None

            

            return { originals = e.originals; vertexShader = vs; tessControlShader = tcs; tessEvalShader = tev; geometryShader = gs; fragmentShader = fs }
        }


    open System.Collections.Generic
    open System.Collections.Concurrent
    let private toEffectCache = 
        ConcurrentDictionary<System.Reflection.MethodBase, Reflection.Closure * FShadeEffect>()

    let toEffect (f : 'a -> Expr<'b>) =
        let closure = FShade.Reflection.PartialEvaluator.getClosure f
        let (originalClosure, template) = toEffectCache.GetOrAdd(closure.inner, fun _ -> closure, createEffect f)
        

        if originalClosure = closure then
            template
        else
            match template with
                | Success template ->
                    match tryReplaceUserUniforms originalClosure closure template with
                        | Some e -> Success e
                        | None -> createEffect f
                | Error e ->
                    Error e

    let private shaderWithDebugInfo (s : Option<Shader>) (info : ShaderDebugInfo) =
        match s with
            | Some s -> Some { s with debugInfo = Some info }
            | None -> None

    let private withDebugInfo (newEffect : Effect) (info : ShaderDebugInfo) =
        { vertexShader = shaderWithDebugInfo newEffect.vertexShader info
          tessControlShader = shaderWithDebugInfo newEffect.tessControlShader info
          tessEvalShader = shaderWithDebugInfo newEffect.tessEvalShader info
          geometryShader = match newEffect.geometryShader with | Some (gs,t) -> Some ({ gs with debugInfo = Some info }, t) | _ -> None
          fragmentShader = shaderWithDebugInfo newEffect.fragmentShader info
          originals = newEffect.originals }

    let debugInfoToEffect (info : ShaderDebugInfo) =
        try 
            let quot = Fsi.compileUntyped info.opened info.functionCode
            match quot with 
                | Fsi.FsiSuccess quot ->
                    let mutable result = quot
                    for (t,n,v) in info.closure do
                        let (arg,ret) = FSharpTypeExt.GetFunctionElements(result.GetType())
                        let funType = typedefof<_ -> _>.MakeGenericType [|arg;ret|]

                        let i = funType.GetMethod("Invoke" , [|t|])
                        result <- i.Invoke(result, [|v|])


                    let mi = getMethodInfo <@ toEffect @>
                    let (a,b) = FSharpTypeExt.GetFunctionElements(result.GetType())
                    let mi = mi.MakeGenericMethod [|a; b.GetGenericArguments().[0]|]

                    let rr = mi.Invoke(null, [|result|])
                    let compiled = rr |> unbox<Compiled<Effect,ShaderState>>

                    compile {
                        let! c = compiled

                        return withDebugInfo c info
                    }
                | Fsi.FsiError e ->
                    compile { return! error "%A" e }

        with e ->
            compile { return! error "%A" e }