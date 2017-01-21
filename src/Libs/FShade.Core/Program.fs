﻿
open Microsoft.FSharp.Quotations

open Aardvark.Base
open Aardvark.Base.Monads.State
open FShade
open FShade.Imperative
open System.Reflection

let sink(a,b) = ()

type GLSLBackend private() =
    inherit Compiler.Backend()
    static let instance = GLSLBackend() :> Compiler.Backend

    static member Instance = instance

    override x.TryGetIntrinsicMethod (c : MethodInfo) =
        match c with
            | MethodQuote <@ sink @> _          -> CIntrinsic.simple "sink" |> Some
            | MethodQuote <@ sin @> _           -> CIntrinsic.simple "sin" |> Some
            | MethodQuote <@ cos @> _           -> CIntrinsic.simple "cos" |> Some
            | MethodQuote <@ clamp @> _         -> CIntrinsic.custom "clamp" [2;0;1] |> Some

            | MethodQuote <@ discard @> _         -> CIntrinsic.simple "discard" |> Some
            | MethodQuote <@ emitVertex @> _      -> CIntrinsic.simple "EmitVertex" |> Some
            | MethodQuote <@ restartStrip @> _    -> CIntrinsic.simple "EndPrimitive" |> Some
            | MethodQuote <@ endPrimitive @> _    -> CIntrinsic.simple "EndPrimitive" |> Some

            | _ -> None
    
    override x.TryGetIntrinsicCtor (c : ConstructorInfo) =
        None

type Vertex =
    {
        [<Position>] p : V4d
        [<Semantic("TexCoord")>] tc : V2d
        [<ClipDistance>] clip : float[]
    }

type Fragment =
    {
        [<Color>] color : V4d
        [<Semantic("TexCoord")>] tc : V2d
    }

type UniformScope with
    member x.Trafo : M44d= x?PerModel?Trafo

[<ReflectedDefinition>]
module Bla =
    let test (a : Arr<4 N, int>) =
        for i in 0 .. a.Length - 1 do
            a.[i] <- i
        a.Length

let effectTest() =

    let vert (v : Vertex) =
        vertex {
            return {
                p = uniform.Trafo * v.p
                tc = v.tc
                clip = [| 0.0; 1.0; 2.0 |]
            }
        }

    let frag (v : Vertex) =
        fragment {
            return {
                color = V4d.IIII
                tc = v.tc
            }
        }

    let vert = Shader.ofFunction vert
    let frag = Shader.ofFunction frag
    
    let effect = Effect.ofList [vert; frag]

    Effect.empty
        |> Effect.link ShaderStage.Fragment (Map.ofList ["Colors", typeof<V4d>; "Bla", typeof<V2d>])
        |> Effect.toModule
        |> Linker.compileAndLink GLSLBackend.Instance
        |> GLSL.CModule.glsl  { 
            GLSL.Config.version = System.Version(4,1,0)
            GLSL.Config.locations = false
            GLSL.Config.perStageUniforms = true
            GLSL.Config.uniformBuffers = true 
        }
        |> printfn "%s"

    effect
        |> Effect.link ShaderStage.Fragment (Map.ofList ["Colors", typeof<V4d>])
        |> Effect.toModule
        |> Linker.compileAndLink GLSLBackend.Instance
        |> GLSL.CModule.glsl  { 
            GLSL.Config.version = System.Version(4,1,0)
            GLSL.Config.locations = false
            GLSL.Config.perStageUniforms = true
            GLSL.Config.uniformBuffers = true 
        }
        |> printfn "%s"

type Vertex1 =
    {
        [<Position>] p1 : V4d
    }

type Vertex2 =
    {
        [<Position>] p2 : V4d
        [<Semantic("Coord")>] c2 : V2d
    }

let composeTest() =
    let a (v : Point<Vertex1>) =
        triangle {
            yield {
                p1 = 1.0 * uniform.Trafo * v.Value.p1
            }

            yield {
                p1 = 2.0 * uniform.Trafo * v.Value.p1
            }

            yield {
                p1 = 3.0 * uniform.Trafo * v.Value.p1
            }
        }

    let b (v : Vertex2) =
        vertex {
            if 1 + 3 < 10 then
                return {
                    p2 = 10.0 * v.p2
                    c2 = V2d(1.0, 4.0) + v.c2
                }
            else
                return {
                    p2 = 3.0 * v.p2
                    c2 = V2d.II + v.c2
                }
        }

    let c (v : Vertex2) =
        fragment {
            return V4d.IOOI
        }

    let sa = Effect.ofFunction a
    let sb = Effect.ofFunction b
    let sc = Effect.ofFunction c



    Effect.compose [sa; sb]
        |> Effect.link ShaderStage.Geometry (Map.ofList [Intrinsics.Position, typeof<V4d>; "Coord", typeof<V2d>; "Hugo", typeof<V2d>])
        |> Effect.toModule
        |> Linker.compileAndLink GLSLBackend.Instance
        |> GLSL.CModule.glsl  { 
            GLSL.Config.version = System.Version(4,1,0)
            GLSL.Config.locations = false
            GLSL.Config.perStageUniforms = false
            GLSL.Config.uniformBuffers = true 
        }
        |> printfn "%s"




module Crazyness = 
    type Test =
        struct
            val mutable public Value : int
            member x.Bla(a : int) = x.Value <- a
            member x.Blubb(a : int) = x.Value + a
            new(v) = { Value = v }
        end

    [<ReflectedDefinition>]
    let test1() =
        let mutable t = Test(100)
        t.Bla(1)
        t.Blubb(2)

    [<ReflectedDefinition>]
    let test2() =
        let t = Test(100)
        t.Bla(1)
        t.Blubb(2)

    let run() =
        printfn "test1: %A" (test1())   // 'test1: 3'
        printfn "test2: %A" (test2())   // 'test2: 102'

        let def1 = getMethodInfo <@ test1 @> |> Expr.TryGetReflectedDefinition |> Option.get
        let def2 = getMethodInfo <@ test2 @> |> Expr.TryGetReflectedDefinition |> Option.get

        printfn "def1: %A" def1
        printfn "def2: %A" def2



[<EntryPoint>]
let main args =
    composeTest()
    System.Environment.Exit 0

//    effectTest()
//    System.Environment.Exit 0


    let optimized = 
        Optimizer.eliminateDeadCode 
            <@
                // long dependency chain test
                // needs 4 iterations in fixpoint search when a is used
                // a <- b <- c <- d <- a ...
                let mutable a = 0
                let mutable b = 0
                let mutable c = 0
                let mutable d = 0
                for i in 0 .. 2 .. 10 do
                    a <- a + b
                    b <- b + c
                    c <- c + d
                    d <- d + a
                sink(a,a)

                // unused y should be remove but side-effect 'z <- z + 1' should remain
                let mutable y = 0 
                let mutable z = 1
                if (z <- z + 1; y < 10) then
                    y <- 10
                else
                    y <- 100
                sink(z,z)


                // since t is unused it should be removed
                let mutable t = 0
                let mutable s = 0
                while (t <- t + 1; s < 10) do
                    s <- s + 1
                sink(s,s)

                // fun fact: do-while looks like:
                let mutable t = 0
                while (
                        t <- t + 1
                        t < 10
                ) do ()

                // array should remain
                let mutable x = 0
                let arr = [| V2i.Zero; V2i.Zero; V2i.Zero; V2i.Zero |]
                for i in 3 .. -1 .. 0 do
                    arr.[i].X <- i

                sink(arr, arr)

                let mutable bla = Arr<4 N, int> [| 1;2;3;4 |]
                let cnt = Bla.test(bla)
                sink(bla, bla)


                // Dot could modify r here (since this is always byref)
                let mutable r = V2d.II
                let test = r.Dot(r)
                sink(r,r)

            @>
    
    let entry =
        Expr.Lambda(Var("unitVar", typeof<unit>), optimized)
            |> Module.ofLambda "test"

//    let entry =
//        Module.ofLambda "test" <@ fun () ->
//            let mutable a = 0
//            let mutable b = 0
//            for i in 8 .. -1 .. 0 do
//                a <- a + 1
//                b <- b + 1
//            a
//        @>

    let entry =
        entry
            |> Linker.compileAndLink GLSLBackend.Instance
            |> GLSL.CModule.glsl  { 
                GLSL.Config.version = System.Version(4,1,0)
                GLSL.Config.locations = false
                GLSL.Config.perStageUniforms = true
                GLSL.Config.uniformBuffers = true 
            }

    printfn "%s" entry
    0

