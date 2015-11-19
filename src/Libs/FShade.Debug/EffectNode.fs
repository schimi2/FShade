namespace FShade.Debug

open FShade.Compiler
open FShade

[<AutoOpen>]
module Core =

    [<ReferenceEquality>]
    type EffectNode = { name : string; read : unit -> FShadeEffect; write : FShadeEffect -> unit }





