
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices
open System
open System.IO
open System.Text


module FShadeService =
    open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
    open System.IO
    open System.Reflection
    open FShade.Compiler
    open FShade
    open Microsoft.FSharp.Quotations
    open Aardvark.Base

    type ErrorSeverity =
        | Warning = 0
        | Error = 1

    type ErrorInfo =
        { startLine : int; startCol : int; endLine : int; endCol : int; message : string; severity : ErrorSeverity; subcategory : string }
    
    type CompileModuleResult =
        | CompileModuleSuccess of appDomain : IDisposable * moduleType : Type * warnings : list<ErrorInfo>
        | CompileModuleError of errors : list<ErrorInfo>



    let private toErrorInfo (f : FSharpErrorInfo) =
        let sev =
            match f.Severity with
                | FSharpErrorSeverity.Error -> ErrorSeverity.Error
                | FSharpErrorSeverity.Warning -> ErrorSeverity.Warning

        { startLine = f.StartLineAlternate; startCol = f.StartColumn; endLine = f.EndLineAlternate; endCol = f.EndColumn; message = f.Message; severity = sev; subcategory = f.Subcategory}

    let private sysLib nm = 
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
            @"\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\" + nm + ".dll"
        else
            let sysDir = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
            let (++) a b = System.IO.Path.Combine(a,b)
            sysDir ++ nm + ".dll" 

    let private fsCore4310() = 
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then // file references only valid on Windows 
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
            @"\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.1.0\FSharp.Core.dll"  
        else 
            sysLib "FSharp.Core"

    


    type private ShaderCompiler() =
        static do
            AppDomain.CurrentDomain.add_ReflectionOnlyAssemblyResolve(fun s e ->
                try
                    Assembly.ReflectionOnlyLoad(e.Name)
                with _ ->
                    null
            )
    
        member x.Compile(ass : byte[], typeName : string, composition : string[]) =
            let composition = composition |> Array.toList
            let ass = Assembly.Load(ass)
            let t = ass.GetType(typeName)

            let methods = composition |> List.map t.GetMethod

            if methods |> List.forall (fun m -> m <> null) then
                let definitions = methods |> List.map Microsoft.FSharp.Quotations.Expr.TryGetReflectedDefinition

                if definitions |> List.forall Option.isSome then
                    let definitions = definitions |> List.map Option.get

                    let shaders = definitions |> List.map lambdaToShader

                    let effects = definitions |> List.map (fun e ->
                        compile {
                            let! s = lambdaToShader e
                            match s with
                                | [s] ->
                                    let effect = toEffectInternal s
                                    return effect
                                | _ ->
                                    return! error "functions can only yield one shader"
                        }
                    )

                    let composed = compose effects

                    GLES.compileEffect composed
                else
                    Error "could not find definitions for some of the specified functions"

            else
                Error "could not find some of the specified functions"
                


    type Fsc() =
        let scs = SimpleSourceCodeServices()

        let newNamespace() =
            let g = Guid.NewGuid()
            g.ToByteArray() |> Array.map (sprintf "%02X") |> String.concat "" |> sprintf "Generated_%s"

        member x.CompileEffect (moduleName : string) (code : string) (composition : list<string>) =
            let ns = newNamespace()
            
            let temp = Path.ChangeExtension(ns, ".fs")
            let assPath = Path.ChangeExtension(temp, ".dll")

            let code = "namespace " + ns + "\r\n[<ReflectedDefinition>]\r\n" + code
            let lineOffset = 2

            File.WriteAllText(temp, code)

            let errors, exitCode = 
                scs.Compile [| 
                    "--simpleresolution"; 
                    "--optimize-"; 
                    "--noframework"; 
                    "--fullpaths"; 
                    "--target:library"; 
                    "-o"; assPath; 
                    "-a"; temp; 
                    "-r"; fsCore4310()
                    "-r"; sysLib "System.Drawing"
                    "-r"; "Aardvark.Base.dll"
                    "-r"; "Aardvark.Base.TypeProviders.dll"
                    "-r"; "Aardvark.Base.FSharp.dll"
                    "-r"; "FShade.Compiler.dll"
                    "-r"; "FShade.dll"
                |]


            let errors = 
                errors |> Array.toList 
                       |> List.map toErrorInfo
                       |> List.map (fun err -> { err with startLine = err.startLine - lineOffset; endLine = err.endLine - lineOffset})

            if exitCode = 0 then

                let d = AppDomain.CreateDomain(ns)
                let data = File.ReadAllBytes(assPath)
                //File.Delete(assPath)

                let instance = d.CreateInstanceAndUnwrap(Assembly.GetAssembly(typeof<ShaderCompiler>).FullName, typeof<ShaderCompiler>.FullName) |> unbox<ShaderCompiler>
                let res = instance.Compile(data, ns + "." + moduleName, composition |> List.toArray)
                AppDomain.Unload(d)

                match res with
                    | Success(uniforms, code) ->
                        Left (uniforms, code)
                    | Error e ->
                        Right ({ startLine = -1; startCol = -1; endLine = -1; endCol = -1; message = e; severity = ErrorSeverity.Error; subcategory = "compile"}::errors)


            else
                Right errors


type FsiSession() =
    // Intialize output and input streams
    let sbOut = new StringBuilder()
    let sbErr = new StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(sbOut)
    let errStream = new StringWriter(sbErr)

    // Build command line arguments & start FSI session
    let argv = [| @"C:\Program Files (x86)\Microsoft SDKs\F#\3.1\Framework\v4.0\fsi.exe" |]
    let allArgs = Array.append argv [|"--noninteractive"|]

    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    let fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream)  

    let tokenizer = FSharpSourceTokenizer([], "test.fsx")

    let identToken = Parser.tagOfToken(Parser.token.IDENT("")) 
    let lines (str : string) =
        str.Split([|System.Environment.NewLine|], StringSplitOptions.None)

    let getLine (str : string) (index : int) =
        if index >= 0 then
            let mutable count = 0
            let mutable last = 0
            let mutable current = str.IndexOf(System.Environment.NewLine, last)

            while last >= 0 && count < index do
                if current >= 0 then
                    let next = str.IndexOf(System.Environment.NewLine, current + 1)
                    last <- current + System.Environment.NewLine.Length
                    current <- next
                    count <- count + 1
                else
                    last <- -1
                    current <- -1

            if last >= 0 then
                if current < 0 then
                    str.Substring(last)
                else
                    str.Substring(last, current - last)
            else
                raise <| System.IndexOutOfRangeException()
        else
            raise <| System.IndexOutOfRangeException()

    member x.References(r : list<string>) =
        for r in r do
            let c = "#r @\"" + r + "\""
            fsiSession.EvalInteraction c

    member x.Check(code : string) =
        let (parse, check,_) = fsiSession.ParseAndCheckInteraction(code)

        check.Errors

    member x.GetToolTip (code : string) (line : int) (col : int) =
        let (parse, check,_) = fsiSession.ParseAndCheckInteraction(code)
        let lineText = getLine code (line - 1)
        
        let tok = tokenizer.CreateLineTokenizer(lineText)
        let rec tryFindToken (col : int) state =
            match tok.ScanToken(state) with
                | Some tok, state ->
                    if tok.LeftColumn <= col && tok.RightColumn >= col then
                        Some tok
                    else
                        tryFindToken col state
                | None, _ ->
                    None

        match tryFindToken (col - 1) 0L with
            | Some t ->
                let token = lineText.Substring(t.LeftColumn, 1 + t.RightColumn - t.LeftColumn)
                check.GetToolTipTextAlternate(line, col, lineText, [token], t.Tag) |> Async.RunSynchronously
            | _ ->
                failwith "sadsad"


let code =
    """module A =
    open Aardvark.Base
    open FShade.Compiler
    open FShade
    type Vertex = { [<Semantic("Positions")>] pos : V4d }

    let a (x : Vertex) =
        vertex {
            return { pos = (uniform?ModelTrafo : M44d) * x.pos}
        }

    let b (x : Vertex) =
        fragment {
            return V4d.IIII
        }
    """


open Aardvark.Base

[<EntryPoint>]
let main argv =
    
    let fsc = FShadeService.Fsc()
    let res = fsc.CompileEffect "A" code ["a"; "b"]

    match res with
        | Left(u,c) ->
            printfn "%A" c

        | Right err ->
            printfn "ERROR: %A" err

    let iter = 1000
    let sw = System.Diagnostics.Stopwatch()

    for i in 1..iter do
        System.GC.Collect()
        System.GC.WaitForFullGCApproach() |> ignore
        System.GC.WaitForFullGCComplete() |> ignore
        sw.Start()
        fsc.CompileEffect "A" code ["a"; "b"] |> ignore
        sw.Stop()

        printfn "%.1f%%: %Ams" (100.0 * float i / float iter) (sw.Elapsed.TotalMilliseconds / float i)

    printfn "took: %Ams" (sw.Elapsed.TotalMilliseconds / float iter)





    System.Environment.Exit(0)
    let checker = FSharpChecker.Create(keepAssemblyContents=true)

    let parseAndCheckSingleFile (input) = 
        let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(),"fsx")  


        
        let input = "#r @\"E:\\Development\\FShade\\src\\packages\\FSharp.Compiler.Service.0.0.82\\lib\\net45\\FSharp.Compiler.Service.dll\"\r\n" + input
        File.WriteAllText(file, input)
        // Get context representing a stand-alone (script) file
        let projOptions = 
            checker.GetProjectOptionsFromScript(file, input)
            |> Async.RunSynchronously

        //let projOptions : FSharpProjectOptions = { projOptions with ReferencedProjects = [|"E:\\Development\\FShade\\src\\packages\\FSharp.Compiler.Service.0.0.82\\lib\\net45\\FSharp.Compiler.Service.dll", Unchecked.defaultof<_>|]}

//        let projOptions =
//            checker.GetProjectOptionsFromCommandLineArgs("project.fsproj",
//                [|
//                    //"--noframework"
//                    "--warn:3"
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\System.Numerics.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\mscorlib.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\FSharp\\.NETFramework\\v4.0\\4.3.1.0\\FSharp.Core.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\System.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\System.Xml.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\System.Runtime.Remoting.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\System.Runtime.Serialization.Formatters.Soap.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\System.Data.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\System.Drawing.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\System.Core.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\System.Web.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\System.Web.Services.dll\""
//                    //"-r:\"C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.0\\System.Windows.Forms.dll\""
//                    //"-r:\"C:\\Users\\Schorsch\\AppData\\Local\\Temp\\FSharp.Compiler.Interactive.Settings.dll\""
//                    //"-r:\"E:\\Development\\FShade\\src\\packages\\FSharp.Compiler.Service.0.0.82\\lib\\net45\\FSharp.Compiler.Service.dll\""
//                |])

        checker.ParseAndCheckProject(projOptions) 
        |> Async.RunSynchronously

    let res = parseAndCheckSingleFile(System.IO.File.ReadAllText(System.IO.Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__)))

    printfn "Errors:"
    for e in res.Errors do
        printfn "%A" e

    let checkedFile = res.AssemblyContents.ImplementationFiles.[0]

    let rec printDecl prefix d = 
        match d with 
        | FSharpImplementationFileDeclaration.Entity (e,subDecls) -> 
            printfn "%sEntity %s was declared and contains %d sub-declarations" prefix e.CompiledName subDecls.Length
            for subDecl in subDecls do 
                printDecl (prefix+"    ") subDecl
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v,vs,e) -> 
            printfn "%sMember or value %s was declared" prefix  v.CompiledName
        | FSharpImplementationFileDeclaration.InitAction(e) -> 
            printfn "%sA top-level expression was declared" prefix 

    for d in checkedFile.Declarations do 
        printDecl "" d

    (*
    let a : int = 1000
    let s = FsiSession()
    s.References [@"E:\Development\FShade\src\packages\FSharp.Compiler.Service.0.0.82\lib\net45\FSharp.Compiler.Service.dll"]
    let code = System.IO.File.ReadAllText(System.IO.Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    let res = s.Check code
    let tt = s.GetToolTip code 66 21
    printfn "%A" tt

    for r in res do
        printfn "error at %d:%d: %A\n" r.StartLineAlternate r.StartColumn r.Message
        *)
    0