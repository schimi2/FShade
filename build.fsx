#I @"packages/FAKE/tools/"
#r @"FakeLib.dll"


open Fake
open Fake.Git
open Fake.FSharpFormatting

let projects = !! "**/*.fsproj"


for p in projects do
    let n = fileNameWithoutExt p
    Target n (fun () -> MSBuildRelease null "Build" [p] |> ignore)


Target "RestorePackages" (fun () ->
    RestorePackages()
)

Target "BeforeBuild" (fun () -> ())

let (&&=) (t : string) (targets : list<string>) =
    Target t id
    let res = targets |> List.fold (fun s t -> s ==> t) "BeforeBuild"
    res ==> t


Target "Clean" (fun () ->
    DeleteDirs ["Bin/Release"; "Bin/Debug"]
) 


"Core" &&= 
    ["FShade.Compiler"; "FShade"; "FShade.Debug"]

"Service" &&= 
    ["Core"; "FSCC"; "FSCC.Service"]

"Demo" &&= 
    ["Core"; "FShade.DemoRenderer"; "FShade.Demo"]

"All" &&= 
    ["Core"; "Service"; "Demo"]

RunTargetOrDefault "All"
