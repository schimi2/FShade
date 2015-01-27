
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

    let private local nm =
        Path.Combine(Environment.CurrentDirectory, nm + ".dll")


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

            Assembly.LoadFile (sysLib "System.Drawing") |> ignore
            Assembly.LoadFile (local "Aardvark.Base") |> ignore
            Assembly.LoadFile (local "Aardvark.Base.TypeProviders") |> ignore
            Assembly.LoadFile (local "Aardvark.Base.FSharp") |> ignore
            Assembly.LoadFile (local "FShade.Compiler") |> ignore
            Assembly.LoadFile (local "FShade") |> ignore
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
                


    type Service() =
        let scs = SimpleSourceCodeServices()

        let newNamespace() =
            let g = Guid.NewGuid()
            g.ToByteArray() |> Array.map (sprintf "%02X") |> String.concat "" |> sprintf "Generated_%s"


        let compile (ass : Assembly) (typeName : string) (composition : list<string>) =
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

        member x.CompileEffect (moduleName : string) (code : string) (composition : list<string>) =
            let ns = newNamespace()
            
            let temp = Path.ChangeExtension(ns, ".fs")
            let assPath = Path.ChangeExtension(temp, ".dll")

            let code = "namespace " + ns + "\r\n#nowarn \"1178\"\r\n[<ReflectedDefinition>]\r\n" + code
            let lineOffset = 3

            File.WriteAllText(temp, code)

            Log.startTimed "compiling %s" ns
            let errors, exitCode = 
                scs.Compile [| 
                    "--simpleresolution"; 
                    "--optimize-"; 
                    "--noframework"; 
                    "--fullpaths"; 
                    "--target:library"; 
                    "--warn:3"
                    "--warnaserror:76"
                    "--nowarn:1178"
                    "--vserrors"
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

//            let args =
//                [| 
//                    "--simpleresolution"; 
//                    "--optimize-"; 
//                    "--noframework"; 
//                    "--fullpaths"; 
//                    "--target:library"; 
//                    "-o"; assPath; 
//                    "-a"; temp; 
//                    "-r"; fsCore4310()
//                    "-r"; sysLib "System.Drawing"
//                    "-r"; "Aardvark.Base.dll"
//                    "-r"; "Aardvark.Base.TypeProviders.dll"
//                    "-r"; "Aardvark.Base.FSharp.dll"
//                    "-r"; "FShade.Compiler.dll"
//                    "-r"; "FShade.dll"
//                |]
//
//            let errors, exitCode, assembly =
//                scs.CompileToDynamicAssembly(args, execute = None)

            Log.stop()


            let errors = 
                errors |> Array.toList 
                       |> List.map toErrorInfo
                       |> List.map (fun err -> { err with startLine = err.startLine - lineOffset; endLine = err.endLine - lineOffset})

            if exitCode = 0 then

                //let res = compile assembly.Value (ns + "." + moduleName) composition


                let d = AppDomain.CreateDomain(ns)
                let data = File.ReadAllBytes(assPath)
                //File.Delete(assPath)

                let instance = d.CreateInstanceAndUnwrap(Assembly.GetAssembly(typeof<ShaderCompiler>).FullName, typeof<ShaderCompiler>.FullName) |> unbox<ShaderCompiler>
                Log.startTimed "translating %s" ns
                let res = instance.Compile(data, ns + "." + moduleName, composition |> List.toArray)
                Log.stop()
                AppDomain.Unload(d)

                match res with
                    | Success(uniforms, code) ->
                        Left (uniforms, code, errors)
                    | Error e ->
                        Right ({ startLine = -1; startCol = -1; endLine = -1; endCol = -1; message = e; severity = ErrorSeverity.Error; subcategory = "compile"}::errors)


            else
                Right errors


    type A = { pos : V3d }

    type FsiSession() =
        // Intialize output and input streams
        let sbOut = new StringBuilder()
        let sbErr = new StringBuilder()
        let inStream = new StringReader("")
        let outStream = new StringWriter(sbOut)
        let errStream = new StringWriter(sbErr)

        // Build command line arguments & start FSI session
        let argv = [| @"C:\Program Files (x86)\Microsoft SDKs\F#\3.1\Framework\v4.0\fsiAnyCpu.exe" |]
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

        do
            let defaultRefs = ["System.Drawing.dll"; "Aardvark.Base.dll"; "Aardvark.Base.TypeProviders.dll"; "Aardvark.Base.FSharp.dll"; "FShade.Compiler.dll"; "FShade.dll"]
            for r in defaultRefs do
                let c = "#r @\"" + r + "\""
                fsiSession.EvalInteraction c
            sbErr.Clear() |> ignore
            sbOut.Clear() |> ignore

        member x.References(r : list<string>) =
            for r in r do
                let c = "#r @\"" + r + "\""
                fsiSession.EvalInteraction c

        member x.Check(code : string) =
            let (parse, check,_) = fsiSession.ParseAndCheckInteraction(code)

            check.Errors |> Array.toList |> List.map toErrorInfo

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
                    let (FSharpToolTipText(elements)) = check.GetToolTipTextAlternate(line, col, lineText, [token], t.Tag) |> Async.RunSynchronously

                    let elements = 
                        elements |> List.collect (fun t ->
                            match t with
                                | FSharpToolTipElement.Single(t, _) -> [t]
                                | FSharpToolTipElement.Group(elements) -> elements |> List.map fst
                                | _ -> []
                        )

                    elements

                | _ ->
                    failwith "sadsad"

    
    open System.Net
    open System.Net.Sockets


    type CompileRequest = { code : string; composition : list<string> }
    type CompileResponse = { glsl : string; errors : list<ErrorInfo> }
    type TooltipRequest = { code : string; row : int; column : int }


    type HttpService(port : int) =
        let pickler = Nessos.FsPickler.Json.FsPickler.CreateJson(true, true)
        let fsc = Service()
        let fsi = FsiSession()

        let l = new HttpListener()
        do l.Prefixes.Add(sprintf "http://localhost:%d/" port)
           l.Start()

        let empty = System.Text.ASCIIEncoding.UTF8.GetBytes "<html><head><title>not found</title></head><body><h1>not found</h1></body></html>"

        let run() =
            async {
                while true do
                    let! ctx = l.GetContextAsync() |> Async.AwaitTask

                    let path = ctx.Request.Url.LocalPath

                    ctx.Response.Headers.Add("Access-Control-Allow-Credentials", "true")
                    ctx.Response.Headers.Add("Access-Control-Allow-Origin", "*")
                    ctx.Response.Headers.Add("Access-Control-Origin", "*")

                    if ctx.Request.HttpMethod = "POST" && path = "/" then
                        let reader = new System.IO.StreamReader(ctx.Request.InputStream, System.Text.ASCIIEncoding.ASCII)
                        let data = reader.ReadToEnd()


                        try 
                            let request : CompileRequest = pickler.UnPickleOfString data
                            let code = request.code.Replace("\t", "    ")

                            match fsc.CompileEffect "A" code request.composition with
                                | Left(_,code,errors) ->
                                    ctx.Response.StatusCode <- 200
                                    ctx.Response.ContentType <- "text/html"

                                    let response = { glsl = code; errors = errors }
                                    let arr = pickler.Pickle response
                                    ctx.Response.OutputStream.Write(arr, 0, arr.Length)
                                    ctx.Response.OutputStream.Close()
                                | Right errors ->
                                    ctx.Response.StatusCode <- 200
                                    ctx.Response.ContentType <- "text/html"

                                    let response = { glsl = ""; errors = errors }
                                    let arr = pickler.Pickle response
                                    ctx.Response.OutputStream.Write(arr, 0, arr.Length)
                                    ctx.Response.OutputStream.Close()
                        with _ ->
                            ctx.Response.StatusCode <- 500
                            ctx.Response.ContentType <- "text/html"
                            ctx.Response.OutputStream.Write(empty, 0, empty.Length)
                            ctx.Response.OutputStream.Close()
                    elif ctx.Request.HttpMethod = "POST" && path = "/tooltip/" then
                        let reader = new System.IO.StreamReader(ctx.Request.InputStream, System.Text.ASCIIEncoding.ASCII)
                        let data = reader.ReadToEnd()


                        try 
                            let request : TooltipRequest = pickler.UnPickleOfString data
                            let code = request.code.Replace("\t", "    ").Replace("\n", System.Environment.NewLine)
                            let tt = fsi.GetToolTip code request.row request.column

                            ctx.Response.StatusCode <- 200
                            ctx.Response.ContentType <- "text/html"
                            let arr = pickler.Pickle tt
                            ctx.Response.OutputStream.Write(arr, 0, arr.Length)
                            ctx.Response.OutputStream.Close()

                        with e ->
                            ctx.Response.StatusCode <- 500
                            ctx.Response.ContentType <- "text/html"
                            ctx.Response.OutputStream.Write(empty, 0, empty.Length)
                            ctx.Response.OutputStream.Close()
                    elif ctx.Request.HttpMethod = "POST" && path = "/check/" then
                        let reader = new System.IO.StreamReader(ctx.Request.InputStream, System.Text.ASCIIEncoding.ASCII)
                        let code = reader.ReadToEnd()
                        let code = code.Replace("\t", "    ")

                        let errors = fsi.Check code
                        ctx.Response.StatusCode <- 200
                        ctx.Response.ContentType <- "text/html"

                        let response = errors
                        let arr = pickler.Pickle response
                        ctx.Response.OutputStream.Write(arr, 0, arr.Length)
                        ctx.Response.OutputStream.Close()
                    else
                        ctx.Response.StatusCode <- 404
                        ctx.Response.ContentType <- "text/html"
                        ctx.Response.OutputStream.Write(empty, 0, empty.Length)
                        ctx.Response.OutputStream.Close()


            }

        member x.Start() =
            run() |> Async.StartAsTask |> ignore

        member x.Run() =
            run() |> Async.RunSynchronously 


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
    
    let s = FShadeService.HttpService(1337)
    s.Run()

    let fsc = FShadeService.Service()
    let fsi = FShadeService.FsiSession()

    let check = fsi.GetToolTip code 9 62
    printfn "%A" check

    let res = fsc.CompileEffect "A" code ["a"; "b"]

    match res with
        | Left(u,c,e) ->
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
        //fsi.GetToolTip code 9 62 |> ignore
        //fsi.Check code |> ignore
        fsc.CompileEffect "A" code ["a"; "b"] |> ignore
        sw.Stop()

        Log.line "%.1f%%: %Ams" (100.0 * float i / float iter) (sw.Elapsed.TotalMilliseconds / float i)

    Log.line "took: %Ams" (sw.Elapsed.TotalMilliseconds / float iter)




    0