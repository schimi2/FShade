﻿namespace FShade

open System
open Aardvark.Base

//
//module internal HMap =
//    let union (l : HMap<'a, 'b>) (r : HMap<'a, 'b>) =
//        let mutable res = l
//        for (k,v) in HMap.toSeq r do
//            res <- HMap.add k v res
//
//        res


type IFunctionSignature =
    interface end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FunctionSignature =
    open System.Reflection
    open Aardvark.Base.IL

    [<AutoOpen>]
    module Implementation = 
        type ClosureArg =
            | CArgument of int
            | CField of FieldInfo
            | CValue of obj

        type Argument =
            | Value of obj
            | Argument of int

        let rec (|CallClosure|_|) (il : list<Instruction>) =
            match il with

                | [ Call(mi); Ret ] 
                | [ Tail; Call(mi); Ret ] ->
                    Some([], mi)

                | (Nop | Start) :: CallClosure(fields, meth) -> 
                    Some(fields, meth)

                | Ldarg 0 :: Ldfld f :: CallClosure(fields, meth) ->
                    Some (CField f :: fields, meth)

                | LdNull :: CallClosure(fields, meth) ->
                    Some (CValue null :: fields, meth)

                | Ldfld f :: CallClosure(fields, meth) when f.IsStatic ->
                    let v = f.GetValue(null)
                    Some (CValue v :: fields, meth)

                | Call mi :: CallClosure(fields, meth) when mi.IsStatic ->
                    if mi.GetParameters().Length = 0 then
                        let v = mi.Invoke(null, [||])
                        Some (CValue v :: fields, meth)
                    else
                        None


                | Ldarg i :: CallClosure(fields, meth) ->
                    Some (CArgument (i - 1) :: fields, meth)

                | LdConst c :: CallClosure(fields, meth)  ->    
                    let value = 
                        match c with
                            | Int8 v -> v :> obj
                            | UInt8 v -> v :> obj
                            | Int16 v -> v :> obj
                            | UInt16 v -> v :> obj
                            | Int32 v -> v :> obj
                            | UInt32 v -> v :> obj
                            | Int64 v -> v :> obj
                            | UInt64 v -> v :> obj
                            | Float64 v -> v :> obj
                            | Float32 v -> v :> obj
                            | NativeInt v -> v :> obj
                            | UNativeInt v -> v :> obj
                            | String v -> v :> obj
                    
                    Some (CValue value :: fields, meth)
                    

                | _ -> None

        let rec extractCall (target : obj) (mi : MethodBase) (args : list<Argument>) =
            if mi.IsStatic && mi.Name = "InvokeFast" then
                match args with
                    | Value t :: rest when not (isNull t) ->
                        let targetType = t.GetType()

                        let cnt = List.length rest

                        let invoke =
                            targetType.GetMethods()
                                |> Seq.tryFind (fun mi -> mi.Name = "Invoke" && mi.GetParameters().Length = cnt)
                                   
                        match invoke with
                            | Some mi -> extractCall t (mi :> MethodBase) rest
                            | None -> (target, mi, args)



                    | _ ->
                        (target, mi, args)

            elif mi.IsStatic then
                (target, mi, args)

                        
            else
                if isNull target then
                    failwith "[FShade] target is null for non-static function"

                match target with
                    | :? Delegate as d ->
                        
                        let impl = d.Method :> MethodBase
                        let target = d.Target
                        let definition = Aardvark.Base.IL.Disassembler.disassemble impl
                        match definition.Body with
                            | CallClosure(argMap, meth) ->
                                let args =
                                    argMap |> List.map (fun a ->
                                        match a with
                                            | CField f -> f.GetValue(target) |> Value
                                            | CArgument i -> args.[i]
                                            | CValue c -> Value c
                                    )
                                if meth.IsStatic then
                                    extractCall null meth args
                                else
                                    match args with
                                        | Value t :: args -> 
                                            extractCall t meth args
                                        | _ ->
                                            (target, impl, args)
                            | _ ->
                                (target, impl, args)
                    | _ ->

                        let targetType = target.GetType()



                        let impl = 
                            if targetType <> mi.DeclaringType then
                                let parameters = mi.GetParameters()
                                let args = parameters |> Array.map (fun p -> p.ParameterType)
                                let flags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
                                let impl = targetType.GetMethod(mi.Name, flags, System.Type.DefaultBinder, args, null)

                                if isNull impl then
                                    Log.warn "[FShade] could not get override for method %A in type %A" mi targetType
                                    null
                                elif mi.IsGenericMethod then
                                    if not impl.IsGenericMethod then
                                        Log.warn "[FShade] could not get override for method %A in type %A" mi targetType
                                        null
                                    else
                                        impl.MakeGenericMethod(mi.GetGenericArguments()) :> MethodBase
                                else
                                    impl :> MethodBase

                            else
                                mi

                        if isNull impl then
                            (target, mi, args)
                        else
                            let definition = Aardvark.Base.IL.Disassembler.disassemble impl
                            match definition.Body with
                                | CallClosure(argMap, meth) ->
                                    let args =
                                        argMap |> List.map (fun a ->
                                            match a with
                                                | CField f -> f.GetValue(target) |> Value
                                                | CArgument i -> args.[i]
                                                | CValue c -> Value c
                                        )
                                    if meth.IsStatic then
                                        extractCall null meth args
                                    else
                                        match args with
                                            | Value t :: args -> 
                                                extractCall t meth args
                                            | _ ->
                                                (target, impl, args)
                                | _ ->
                                    (target, impl, args)

        type Signature =
            {
                target : obj
                mi : MethodBase
                args : list<Argument>
            }
            with interface IFunctionSignature

        type RandomFunctionSignature() =
            let id = Guid.NewGuid()
            interface IFunctionSignature

            member x.Id = id

            override x.GetHashCode() = id.GetHashCode()
            override x.Equals o =
                match o with
                    | :? RandomFunctionSignature as o -> id = o.Id
                    | _ -> false

    let ofFunction (f : 'a -> 'b) =
        try

            let invoke =
                if typeof<'b>.Name.StartsWith "FSharpFunc" then
                    let invokes = f.GetType().GetMethods() |> Array.filter (fun mi -> mi.Name = "Invoke")
                    if invokes.Length = 0 then null
                    else invokes |> Array.maxBy (fun mi -> mi.GetParameters().Length)
                else
                    f.GetType().GetMethod "Invoke"

            if isNull invoke then
                failwithf "[FShade] could not get signature for function %A" f

            let (target, mi, args) = extractCall f invoke (invoke.GetParameters() |> Array.toList |> List.mapi (fun i _ -> Argument (1 + i)))

            let target =
                if isNull target then
                    target
                else
                    let targetType = target.GetType()
                    let targetFields = targetType.GetFields(BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance)
                    if targetFields.Length = 0 then
                        null
                    else
                        target

            { target = target; mi = mi; args = args } :> IFunctionSignature
        with e ->
            Log.warn "[FShade] could not get function signature for %A" f
            Log.warn "[FShade] said: '%s'" e.Message
            RandomFunctionSignature() :> IFunctionSignature

    let tryGetAttribute<'a when 'a :> System.Attribute>(signature : IFunctionSignature) =
        match signature with
            | :? Signature as s -> 
                match s.mi.GetCustomAttributes(typeof<'a>) |> Seq.tryHead with
                    | Some (:? 'a as a) -> Some a
                    | _ -> None
            | _ ->
                None

type UniformScope(parent : Option<UniformScope>, name : string) = 
    static let mutable currentId = 0

    let id = System.Threading.Interlocked.Increment(&currentId)
    let childScopes = System.Collections.Generic.Dictionary<string, UniformScope>()

    member private x.Id = id
    interface IComparable with
        member x.CompareTo o =
            match o with
                | :? UniformScope as o -> id.CompareTo o.Id
                | _ -> failwith "uncomparable"

    override x.GetHashCode() =
        id.GetHashCode()

    override x.Equals(o) =
        match o with
            | :? UniformScope as o -> o.Id = id
            | _ -> false

    member x.Parent = parent
    member x.Name = name
    member x.FullName = 
        let rec build (name : string) (s : UniformScope) =
            match s.Parent with
                | None -> 
                    if name.Length = 0 then "Global"
                    else name
                | Some p ->
                    build (s.Name + name) p

        build "" x

    member x.GetChildScope(n : string) =
        lock childScopes (fun () ->
            match childScopes.TryGetValue n with
                | (true,s) -> 
                    s
                | _ -> 
                    let s = UniformScope(Some x, n)
                    childScopes.[n] <- s
                    s
        )
   
type ISemanticValue =
    abstract member Semantic : string
    abstract member Scope : UniformScope

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type SemanticAttribute(s : string) =
    inherit Attribute()
    member x.Semantic = s

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type InterpolationAttribute(qualifier : InterpolationMode) =
    inherit Attribute()
    member x.Qualifier = qualifier

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type PrimitiveIndexAttribute(index : int) =
    inherit Attribute()
    member x.Index = index

type IShaderBuilder =
    abstract member ShaderStage : ShaderStage
    abstract member OutputTopology : Option<OutputTopology>

type ParameterDescription =
    {
        paramType           : Type
        paramInterpolation  : InterpolationMode
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParameterDescription =

    let inline paramInterpolation (p : ParameterDescription) = p.paramInterpolation
    let inline paramType (p : ParameterDescription) = p.paramType

    let ofType (t : Type) =
        {
            paramType = t
            paramInterpolation = InterpolationMode.Default
        }


module Formats =
    type IFormat = interface end 
    type IFloatingFormat = inherit IFormat
    type ISignedFormat = inherit IFormat
    type IUnsignedFormat = inherit IFormat

    type rgba32f() = interface IFloatingFormat
    type rgba16f() = interface IFloatingFormat
    type rg32f() = interface IFloatingFormat
    type rg16f() = interface IFloatingFormat
    type r11g11b10f() = interface IFloatingFormat
    type r32f() = interface IFloatingFormat
    type r16f() = interface IFloatingFormat

    type rgba16() = interface IFloatingFormat
    type rgb10a2() = interface IFloatingFormat
    type rgba8() = interface IFloatingFormat
    type rg16() = interface IFloatingFormat
    type rg8() = interface IFloatingFormat
    type r16() = interface IFloatingFormat
    type r8() = interface IFloatingFormat

    type rgba16_snorm() = interface IFloatingFormat
    type rgba8_snorm() = interface IFloatingFormat
    type rg16_snorm() = interface IFloatingFormat
    type rg8_snorm() = interface IFloatingFormat
    type r16_snorm() = interface IFloatingFormat
    type r8_snorm() = interface IFloatingFormat

    type rgba32ui() = interface IUnsignedFormat
    type rgba16ui() = interface IUnsignedFormat
    type rgb10a2ui() = interface IUnsignedFormat
    type rgba8ui() = interface IUnsignedFormat
    type rg32ui() = interface IUnsignedFormat
    type rg16ui() = interface IUnsignedFormat
    type rg8ui() = interface IUnsignedFormat
    type r32ui() = interface IUnsignedFormat
    type r16ui() = interface IUnsignedFormat
    type r8ui() = interface IUnsignedFormat

    type rgba32i() = interface ISignedFormat
    type rgba16i() = interface ISignedFormat
    type rgba8i() = interface ISignedFormat
    type rg32i() = interface ISignedFormat
    type rg16i() = interface ISignedFormat
    type rg8i() = interface ISignedFormat
    type r32i() = interface ISignedFormat
    type r16i() = interface ISignedFormat
    type r8i() = interface ISignedFormat




type UniformValue =
    | Attribute of scope : UniformScope * name : string
    | Sampler of textureName : string * SamplerState
    | SamplerArray of array<string * SamplerState>

type UniformParameter =
    {
        uniformName         : string
        uniformType         : Type
        uniformValue        : UniformValue
    }




type ISampler =
    abstract member Texture : ISemanticValue
    abstract member State : SamplerState

type IImage =
    interface end

type ShaderTextureHandle(semantic : string, scope : UniformScope) =
    static member CreateUniform(semantic : string, scope : UniformScope) = ShaderTextureHandle(semantic, scope)
    interface ISemanticValue with
        member x.Semantic = semantic
        member x.Scope = scope


    new() = ShaderTextureHandle(null, Unchecked.defaultof<UniformScope>)       

type TextureMustBeSpecified = TextureMustBeSpecified

type SamplerBaseBuilder() =
    member x.Yield(_) = TextureMustBeSpecified

    [<CustomOperation("texture")>]
    member x.Texture(b : TextureMustBeSpecified, t : ShaderTextureHandle) =
        (t, SamplerState.empty)

    [<CustomOperation("addressU")>]
    member x.AddressU((t : ShaderTextureHandle, h : SamplerState), w : WrapMode) = t,{ h with AddressU = Some w }
             
    [<CustomOperation("addressV")>]
    member x.AddressV((t : ShaderTextureHandle, h : SamplerState), w : WrapMode) = t,{ h with AddressV = Some w }
             
    [<CustomOperation("addressW")>]
    member x.AddressW((t : ShaderTextureHandle, h : SamplerState), w : WrapMode) = t,{ h with AddressW = Some w }
             
    [<CustomOperation("maxAnisotropy")>]
    member x.MaxAnisotropy((t : ShaderTextureHandle, h : SamplerState), a : int) = t,{ h with MaxAnisotropy = Some a }
             
    [<CustomOperation("borderColor")>]
    member x.BorderColor((t : ShaderTextureHandle, h : SamplerState), c : C4f) = t,{ h with BorderColor = Some c }
             
    [<CustomOperation("maxLod")>]
    member x.MaxLod((t : ShaderTextureHandle, h : SamplerState), c : float) = t,{ h with MaxLod = Some c }
             
    [<CustomOperation("minLod")>]
    member x.MinLod((t : ShaderTextureHandle, h : SamplerState), c : float) = t,{ h with MinLod = Some c }
             
    [<CustomOperation("mipLodBias")>]
    member x.MipLodBias((t : ShaderTextureHandle, h : SamplerState), c : float) = t,{ h with MipLodBias = Some c }
             
    [<CustomOperation("filter")>]
    member x.Filter((t : ShaderTextureHandle, h : SamplerState), f : Filter) = t,{ h with Filter = Some f }

    [<CustomOperation("comparison")>]
    member x.Comparison((t : ShaderTextureHandle, h : SamplerState), f : ComparisonFunction) = t,{ h with Comparison = Some f }

[<AutoOpen>]
module UniformExtensions =

    let uniform = UniformScope(None, "Global")


    type internal UniformStuff private() =

        [<ThreadStatic; DefaultValue>]
        static val mutable private current : Option<UniformScope * string>

        static member Push() =
            let old = UniformStuff.current
            UniformStuff.current <- None
            old

        static member Pop(old : Option<_>) =
            let c = UniformStuff.current
            UniformStuff.current <- old
            c

        static member Set(scope : UniformScope, name : string) =
            UniformStuff.current <- Some(scope, name)

    let (?) (s : UniformScope) (name : string) : 'a =
        let t = typeof<'a>

        if typeof<ISemanticValue>.IsAssignableFrom t then

            let creator = t.GetMethod("CreateUniform", [|typeof<string>; typeof<UniformScope>|])
            if creator <> null then
                let result = creator.Invoke(null, [| name :> obj ; s :> obj |])
                result |> unbox
            else
                UniformStuff.Set(s, name)
                Unchecked.defaultof<'a>
                
        elif t = typeof<UniformScope> then
            s.GetChildScope name |> unbox<'a>
        else
            UniformStuff.Set(s, name)
            Unchecked.defaultof<'a>