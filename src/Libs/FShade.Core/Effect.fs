﻿namespace FShade

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative


type Effect internal(shaders : Map<ShaderStage, Shader>) =
        
    let inputToplogy =
        lazy (
            shaders |> Map.toSeq |> Seq.tryPick (fun (_,s) ->
                s.shaderInputTopology
            )
        )

    let first =
        lazy (
            shaders |> Map.toSeq |> Seq.map snd |> Seq.tryHead
        )

    let last =
        lazy (
            shaders |> Map.toSeq |> Seq.map snd |> Seq.tryLast
        )

    let inputs =
        lazy (
            match first.Value with
                | Some shader -> Shader.inputs shader
                | None -> Map.empty
        )

    let outputs =
        lazy (
            match last.Value with
                | Some shader -> Shader.outputs shader
                | None -> Map.empty
        )

    let uniforms =
        lazy (
            shaders 
                |> Map.toSeq 
                |> Seq.map (fun (_,s) -> Shader.uniforms s) 
                |> Seq.fold Map.union Map.empty
        )

    member x.InputToplogy = inputToplogy.Value
    member x.FirstShader = first.Value
    member x.LastShader = last.Value
    member x.Inputs = inputs.Value
    member x.Outputs = outputs.Value
    member x.Uniforms = uniforms.Value

    member x.Shaders            = shaders
    member x.VertexShader       = shaders |> Map.tryFind ShaderStage.Vertex
    member x.TessControlShader  = shaders |> Map.tryFind ShaderStage.TessControl
    member x.TessEvalShader     = shaders |> Map.tryFind ShaderStage.TessEval
    member x.GeometryShader     = shaders |> Map.tryFind ShaderStage.Geometry
    member x.FragmentShader     = shaders |> Map.tryFind ShaderStage.Fragment


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Effect =
    let empty = Effect Map.empty

    let inline vertexShader (effect : Effect) = effect.VertexShader
    let inline tessControlShader (effect : Effect) = effect.TessControlShader
    let inline tessEvalShader (effect : Effect) = effect.TessEvalShader
    let inline geometryShader (effect : Effect) = effect.GeometryShader
    let inline fragmentShader (effect : Effect) = effect.FragmentShader
    
    let inline inputTopology (effect : Effect) = effect.InputToplogy
    let inline firstShader (effect : Effect) = effect.FirstShader
    let inline lastShader (effect : Effect) = effect.LastShader
    let inline firstStage (effect : Effect) = effect.FirstShader |> Option.map Shader.stage
    let inline lastStage (effect : Effect) = effect.LastShader |> Option.map Shader.stage
    let inline inputs (effect : Effect) = effect.Inputs
    let inline outputs (effect : Effect) = effect.Outputs
    let inline uniforms (effect : Effect) = effect.Uniforms

    let ofMap (shaders : Map<ShaderStage, Shader>) =
        for (stage, shader) in Map.toSeq shaders do
            if stage <> shader.shaderStage then 
                failwithf "[FShade] inconsistent shader-map: %A claims to be %A" shader.shaderStage stage

        Effect shaders

    let ofSeq (shaders : seq<Shader>) =
        let mutable map = Map.empty
        for shader in shaders do
            match Map.tryFind shader.shaderStage map with
                | Some prev -> 
                    failwithf "[FShade] conflicting shaders for stage: %A" shader.shaderStage
                | None ->
                    map <- Map.add shader.shaderStage shader map

        Effect map
            
    let inline ofList (shaders : list<Shader>) = 
        ofSeq shaders

    let inline ofArray (shaders : Shader[]) = 
        ofSeq shaders

    let ofShader (shader : Shader) =
        Effect (Map.ofList [shader.shaderStage, shader])

    let ofExpr (inputType : Type) (e : Expr) =
        Shader.ofExpr inputType e |> ofList

    let ofFunction (shaderFunction : 'a -> Expr<'b>) =
        Shader.ofFunction shaderFunction |> ofList


    let inline toMap (effect : Effect) = effect.Shaders

    let toSeq (effect : Effect) =
        effect.Shaders |> Map.toSeq |> Seq.map snd

    let toList (effect : Effect) =
        effect.Shaders |> Map.toList |> List.map snd

    let toArray (effect : Effect) =
        effect.Shaders |> Map.toSeq |> Seq.map snd |> Seq.toArray

    let isEmpty (effect : Effect) = 
        Map.isEmpty effect.Shaders
            
    let tryFindShader (stage : ShaderStage) (effect : Effect) =
        Map.tryFind stage effect.Shaders

    let hasStage (stage : ShaderStage) (effect : Effect) =
        Map.containsKey stage effect.Shaders


    let add (shader : Shader) (effect : Effect) =
        Effect (Map.add shader.shaderStage shader effect.Shaders)

    let remove (stage : ShaderStage) (effect : Effect) =
        Effect (Map.remove stage effect.Shaders)

    let alter (stage : ShaderStage) (update : Option<Shader> -> Option<Shader>) (effect : Effect) =
        match update (tryFindShader stage effect) with
            | Some n -> 
                if stage <> n.shaderStage then
                    failwithf "[FShade] cannot change shader-stage in Effect.alter from %A to %A" stage n.shaderStage

                add n effect

            | None -> 
                remove stage effect

    let addIfNotPresent (stage : ShaderStage) (create : ShaderStage -> Shader) (effect : Effect) =
        alter stage (function Some o -> Some o | None -> Some (create stage)) effect

    let link (stage : ShaderStage) (outputs : Map<string, Type>) (effect : Effect) =
        let rec linkShaders (needed : Map<string, Type>) (l : list<Shader>) =
            match l with
                | [] -> 
                    []

                | current :: before ->
                    if current.shaderStage = ShaderStage.TessControl then

                        let newNeeded =
                            Map.difference needed (Shader.outputs current) |> Map.map (fun _ t -> 
                                match t with
                                    | ArrayOf t -> t
                                    | _ -> t
                            )
                        
                        let newBefore = 
                            linkShaders newNeeded before

                        let desired =
                            needed |> Map.union (Shader.systemOutputs current)

                        let newCurrent = 
                            current |> Shader.mapOutputs (fun writes ->
                                writes |> Map.filter (fun n _ -> desired.ContainsKey n)
                            )

                        newCurrent :: newBefore
                    else
                        let desired =
                            needed |> Map.union (Shader.systemOutputs current) |> Map.remove Intrinsics.SourceVertexIndex

                        let newCurrent = 
                            Shader.withOutputs desired current

                        let newBefore = 
                            linkShaders (Shader.inputs newCurrent) before

                        newCurrent :: newBefore
                             
        effect 
            // add the final desired stage passing all desired
            // outputs (if not yet present)
            |> addIfNotPresent stage (Shader.passing outputs)

            // add an empty vertex shader when none is present
            |> addIfNotPresent ShaderStage.Vertex (Shader.passing Map.empty)

            // link all shaders (backward)
            |> toList
            |> List.rev
            |> linkShaders outputs        
                
            // and create the new effect                              
            |> ofList
                        
    let toModule (effect : Effect) =
        let rec entryPoints (lastStage : Option<Shader>) (shaders : list<Shader>) =
            match shaders with
                | [] -> 
                    []

                | [shader] -> 
                    [ Shader.toEntryPoint lastStage shader None ]

                | shader :: next :: after ->
                    let shaderEntry = Shader.toEntryPoint lastStage shader (Some next) 
                    shaderEntry :: entryPoints (Some shader) (next :: after)

        { entries = entryPoints None (toList effect) }

    let compose2 (l : Effect) (r : Effect) =
        let geometryLeft = l |> toList |> List.filter (fun s -> s.shaderStage < ShaderStage.Fragment)
        let geometryRight = r |> toList |> List.filter (fun s -> s.shaderStage < ShaderStage.Fragment)

        let rec composeToLast (l : list<Shader>) (r : list<Shader>) =
            match l with
                | [] -> r
                | [l] ->
                    match r with
                        | [] -> [l]
                        | rh :: _ ->
                            if l.shaderStage < rh.shaderStage then
                                l :: r
                            else
                                let mutable res = l
                                for r in r do res <- Shader.compose2 res r
                                [ res ]
                | h :: rest ->
                    h :: composeToLast rest r

        let shaders = 
            composeToLast geometryLeft geometryRight

        let shaders = 
            match l.FragmentShader, r.FragmentShader with
                | Some l, Some r -> (Shader.compose2 l r) :: shaders
                | None, Some r -> r :: shaders
                | Some l, None -> l :: shaders
                | None, None -> shaders
                        
        ofList shaders

    let compose (effects : #seq<Effect>) =
        effects |> Seq.fold compose2 empty


        
            






        

