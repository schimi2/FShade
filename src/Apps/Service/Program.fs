
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices
open System
open System.IO
open System.Text

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


[<EntryPoint>]
let main argv =
    
    
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