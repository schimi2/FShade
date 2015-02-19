
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices
open System
open System.IO
open System.Text
open System.Security
open System.Security.Policy
open System.Security.Permissions
open System.Reflection

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
    
    type Symbol =
        { kind : string; displayName : string; fullName : string; parameters : string; returnType : string }

    type CompileModuleResult =
        | CompileModuleSuccess of appDomain : IDisposable * moduleType : Type * warnings : list<ErrorInfo>
        | CompileModuleError of errors : list<ErrorInfo>



    type AppDomain with
        static member CreateChildDomain() =
            let setup = AppDomainSetup()
            setup.ApplicationBase <- System.Environment.CurrentDirectory
            let ev = AppDomain.CurrentDomain.Evidence


            AppDomain.CreateDomain(Guid.NewGuid().ToString(), ev, setup)



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

    [<AllowNullLiteral>]
    type Compiler() =
        inherit MarshalByRefObject()

        let createdFiles = System.Collections.Generic.HashSet<string>()
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

        member x.CreatedFiles = createdFiles |> Seq.toList
    
        member x.CompileEffect (moduleName : string) (code : string) (composition : list<string>) =
            let ns = newNamespace()
            
            let temp = Path.ChangeExtension(ns, ".fs")
            let assPath = Path.ChangeExtension(temp, ".dll")

            let code = "namespace " + ns + "\r\n#nowarn \"1178\"\r\n[<ReflectedDefinition>]\r\n" + code
            let lineOffset = 3

            File.WriteAllText(temp, code)



            //Log.startTimed "compiling %s" ns
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
//
//            let args =
//                [| 
//                    "--simpleresolution"; 
//                    "--optimize-"; 
//                    "--noframework"; 
//                    "--fullpaths"; 
//                    "--target:library"; 
//                    "--warn:3"
//                    "--warnaserror:76"
//                    "--nowarn:1178"
//                    "--vserrors"
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

            File.Delete temp


            let errors = 
                errors |> Array.toList 
                       |> List.map toErrorInfo
                       |> List.map (fun err -> { err with startLine = err.startLine - lineOffset; endLine = err.endLine - lineOffset})

            if exitCode = 0 then
                let assPath = Path.GetFullPath assPath
                createdFiles.Add assPath |> ignore
                let assembly = Assembly.LoadFile assPath |> Some

                let res = compile assembly.Value (ns + "." + moduleName) composition


//                let d = AppDomain.CreateDomain(ns)
//                let data = File.ReadAllBytes(assPath)
                //File.Delete(assPath)

//                let instance = d.CreateInstanceAndUnwrap(Assembly.GetAssembly(typeof<ShaderCompiler>).FullName, typeof<ShaderCompiler>.FullName) |> unbox<ShaderCompiler>
//                Log.startTimed "translating %s" ns
//                let res = instance.Compile(data, ns + "." + moduleName, composition |> List.toArray)
//                Log.stop()
//                AppDomain.Unload(d)

                match res with
                    | Success(uniforms, code) ->
                        Left (uniforms, code, errors)
                    | Error e ->
                        Right ({ startLine = -1; startCol = -1; endLine = -1; endCol = -1; message = e; severity = ErrorSeverity.Error; subcategory = "compile"}::errors)


            else
                Right errors

    type FsiSession() =
       
        inherit MarshalByRefObject()

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

        member x.GetQuotations (moduleName : string) (functions : list<string>) =
            match fsiSession.EvalExpression(sprintf "typeof<%s.Marker>.DeclaringType" moduleName) with
                | Some v ->
                    match v.ReflectionValue with
                        | :? Type as t ->
                            let methods = functions |> List.map t.GetMethod

                            if methods |> List.forall (fun mi -> mi <> null) then
                                let defs = methods |> List.map Expr.TryGetReflectedDefinition

                                if defs |> List.forall Option.isSome then
                                    defs |> List.map Option.get |> Some
                                else
                                    None
                            else
                                None

                        | _ ->
                            None
                | None -> None

        member x.CompileShader (moduleName : string) (functions : list<string>) =
            match x.GetQuotations moduleName functions with
                | Some definitions ->
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
                | None ->
                    Error "could not find at least one of the shaders in the composition"



        member x.Eval(code : string) =
            match fsiSession.EvalExpression(code) with
                | Some v ->
                    let va = v.ReflectionValue
                    Some va
                | None -> None

        member x.Interact(code : string) =
            fsiSession.EvalInteraction(code)

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

        member x.GetCompletions (code : string) (line : int) (col : int) =
            let (parse, check, asdas) = fsiSession.ParseAndCheckInteraction(code)
            let lineText = getLine code (line - 1)
            async {
                let! b = check.GetDeclarationSymbols(Some parse, line, col, lineText, [], "")
                let! a = check.GetAllUsesOfAllSymbolsInFile()
                return a |> Array.tryFind(fun su -> su.RangeAlternate.StartLine >= line),b
            }

    type Service() =
        let mutable useCount = 0
        let l = obj()
        let mutable d : AppDomain = null
        let mutable c : Compiler = null
        let checker = FsiSession()

        let simpleCode = "module A =\r\n    open Aardvark.Base\r\n    open FShade.Compiler\r\n    open FShade\r\n    type Vertex = \r\n        { [<Semantic(\"Positions\")>] pos : V4d \r\n          [<Semantic(\"TexCoord\")>] tc : V2d\r\n          [<Semantic(\"Colors\")>] color : V4d\r\n        }\r\n\r\n    let a (x : Vertex) =\r\n        vertex {\r\n            return { x with pos = (uniform?ModelTrafo : M44d) * x.pos }\r\n        }\r\n\r\n    let b (x : Vertex) =\r\n        fragment {\r\n            return x.color\r\n        }\r\n        \r\n    let c (x : Vertex) =\r\n        fragment {\r\n            return x.color * V4d(x.tc.Y, x.tc.Y, 2.0, 1.0)\r\n        }"


        let createNewCompiler() =
            async {
                let d' = AppDomain.CreateChildDomain()
                let instance = d'.CreateInstanceAndUnwrap(typeof<Compiler>.Assembly.FullName, typeof<Compiler>.FullName) |> unbox<Compiler>
                instance.CompileEffect "A" simpleCode ["a"; "b"; "c"] |> ignore
                

                let files = ref []
                let oldDomain = ref null
                lock l (fun () ->
                    files := if c <> null then c.CreatedFiles else []
                    oldDomain := d
                    d <- d'
                    c <- instance
                    useCount <- 0
                    Log.line "compiler ready"
                )

                if !oldDomain <> null then
                    AppDomain.Unload(!oldDomain)

                for f in !files do
                    try File.Delete f
                    with e -> printfn "%A" e

            }

        do createNewCompiler() |> Async.RunSynchronously

        member x.Check code =
            checker.Check code

        member x.GetCompletions code line col =
            let rec typeName (t : FSharpType) =
//                if t.HasTypeDefinition then
//                    let n = t.TypeDefinition.DisplayName
//                    if t.GenericArguments.Count > 0 then
//                        let suffix = t.GenericArguments |> Seq.map (fun a -> typeName a) |> String.concat "," |> sprintf "<%s>"
//                        n + suffix
//                    else
//                        n
//                else
                    t.Format FSharpDisplayContext.Empty

            async {
                let! (sym, completions) = checker.GetCompletions code line col
                printfn "%A" sym
                return completions |> List.map (fun a -> a |> List.map (fun (c : FSharpSymbol) -> 
                    match c with
                        | :? FSharpMemberOrFunctionOrValue as c ->
                            let kind =
                                if c.IsProperty then "property"
                                elif c.IsActivePattern then "activepattern"
                                elif c.IsEvent then "event"
                                elif c.IsMember then "member"
                                else "field"

                            let name = 
                                if c.GenericParameters.Count > 0 then
                                    let suffix = c.GenericParameters |> Seq.map (fun g -> g.Name) |> String.concat ", " |> sprintf "<%s>"
                                    c.DisplayName + suffix
                                else
                                    c.DisplayName

                            
                            let parameters =
                                c.CurriedParameterGroups 
                                    |> Seq.map (fun g -> 
                                        g |> Seq.map (fun p -> 
                                            let n = match p.Name with | Some n -> n | _ -> "arg"
                                            sprintf "%s : %s" n (typeName p.Type)
                                        ) |> String.concat ", " |> sprintf "(%s)"
                                    ) |> String.concat " "

                          

                            let retType = typeName c.ReturnParameter.Type
                            
                            { kind = kind; displayName = name; fullName = c.FullName; parameters = parameters; returnType = retType }
                        | _ -> 
                            { kind = "unknown"; displayName = c.DisplayName; fullName = c.FullName;  parameters = ""; returnType = "" }
                    ))
 
            } |> Async.RunSynchronously

        member x.GetTooltip code line col =
            checker.GetToolTip code line col

        member x.CompileEffect (moduleName : string) (code : string) (composition : list<string>) =
            if useCount = 5 then
                Log.line "creating new compiler instance"
                createNewCompiler() |> Async.StartAsTask |> ignore
            useCount <- useCount + 1

            lock l (fun () ->
                Log.startTimed "compile"
                let res = c.CompileEffect moduleName code composition
                Log.stop()
                res
            ) 




    type QuotationExtractor() =
        let cancel = new System.Threading.CancellationTokenSource()

        let mutable interactionCount = 0
        let l = obj()
        let mutable domain = null
        let mutable fsi = Unchecked.defaultof<FsiSession> //domain.CreateInstanceAndUnwrap(typeof<FsiSession>.Assembly.FullName, typeof<FsiSession>.Name) |> unbox<FsiSession>


        let compileModule (f : FsiSession) (log : bool) (moduleName : string) (code : string) (composition : list<string>) =
            let errors = f.Check(code)

            if errors |> List.exists (fun e -> e.severity = ErrorSeverity.Error) then
                Right errors
            else
                let code = "#nowarn \"1178\"\r\n[<ReflectedDefinition>]\r\n" + code + "\r\n    type Marker = Marker\r\n"
                let lineOffset = 2

                let sw = System.Diagnostics.Stopwatch()
                sw.Start()
                f.Interact code
                let res = f.CompileShader moduleName composition
                sw.Stop()
                if log then Log.line "compile took: %fms" sw.Elapsed.TotalMilliseconds


                match res with
                    | Success(uniforms, code) ->

                        Left (uniforms, code, errors)
                    | Error e ->
                        Right ({ startLine = -1; startCol = -1; endLine = -1; endCol = -1; message = e; severity = ErrorSeverity.Error; subcategory = "compile"}::errors)

        let simpleCode = "module A =\r\n    open Aardvark.Base\r\n    open FShade.Compiler\r\n    open FShade\r\n    type Vertex = \r\n        { [<Semantic(\"Positions\")>] pos : V4d \r\n          [<Semantic(\"TexCoord\")>] tc : V2d\r\n          [<Semantic(\"Colors\")>] color : V4d\r\n        }\r\n\r\n    let a (x : Vertex) =\r\n        vertex {\r\n            return { x with pos = (uniform?ModelTrafo : M44d) * x.pos }\r\n        }\r\n\r\n    let b (x : Vertex) =\r\n        fragment {\r\n            return x.color\r\n        }\r\n        \r\n    let c (x : Vertex) =\r\n        fragment {\r\n            return x.color * V4d(x.tc.Y, x.tc.Y, 2.0, 1.0)\r\n        }"

        let reset() =
            async {
                let d = AppDomain.CreateChildDomain()

                let assName = Assembly.GetAssembly(typeof<FsiSession>).FullName
                let f = d.CreateInstanceAndUnwrap(assName, typeof<FsiSession>.FullName) 


                let f = f |> unbox<FsiSession>
                compileModule f false "A" simpleCode ["a"; "b"; "c"] |> ignore

                let oldDomain = ref null
                lock l (fun () ->   
                    oldDomain := domain
                    domain <- d
                    fsi <- f
                    interactionCount <- 0
                )
                Log.line "FSI running"

                if !oldDomain <> null then
                    AppDomain.Unload(!oldDomain)

                
            }

        do reset() |> Async.RunSynchronously


        member x.StartReset() =
            Async.StartAsTask(reset(), cancellationToken = cancel.Token) |> ignore

        member x.Check(code : string) =
            fsi.Check(code)

        member x.GetTooltip code line col =
            fsi.GetToolTip code line col

        member x.GetCompletions code line col =
            fsi.GetCompletions code line col

        member x.CompileEffect(moduleName : string) (code : string) (composition : list<string>) =
            if interactionCount = 5 then
                Log.line "restarting FSI"
                x.StartReset()
            interactionCount <- interactionCount + 1

            lock l (fun () ->
                compileModule fsi true moduleName code composition
            )
    
    open System.Net
    open System.Net.Sockets


    type CompileRequest = { code : string; composition : list<string> }
    type CompileResponse = { glsl : string; errors : list<ErrorInfo> }
    type TooltipRequest = { code : string; row : int; column : int }


    type HttpService(port : int) =
        let pickler = Nessos.FsPickler.Json.FsPickler.CreateJson(true, true)
        let service = Service()

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

                            match service.CompileEffect "A" code request.composition with
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
                        with e ->
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
                            let tt = service.GetTooltip code request.row request.column

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

                        let errors = service.Check code
                        ctx.Response.StatusCode <- 200
                        ctx.Response.ContentType <- "text/html"

                        let response = errors
                        let arr = pickler.Pickle response
                        ctx.Response.OutputStream.Write(arr, 0, arr.Length)
                        ctx.Response.OutputStream.Close()

                    elif ctx.Request.HttpMethod = "POST" && path = "/completions/" then
                        let reader = new System.IO.StreamReader(ctx.Request.InputStream, System.Text.ASCIIEncoding.ASCII)
                        let data = reader.ReadToEnd()

                        try 
                            let request : TooltipRequest = pickler.UnPickleOfString data
                            let code = request.code.Replace("\t", "    ").Replace("\n", System.Environment.NewLine)
                            let tt = service.GetCompletions code request.row request.column

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
//    
//    Logging.TestLogging.run()
//    System.Environment.Exit(0)

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