// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSCC.Service.Fsi

module Service =
    open System.Net
    open System.Threading
    open System.Threading.Tasks
    open System.IO
    open System.Text
    open System.Text.RegularExpressions
    open FSCC

    let listener = new HttpListener()

    let mutable workersRunning = 0

    let private bounaryRx = Regex "[\r\n]*---.*Boundary.*[\r\n]+Content-Disposition:[ \t]*(?<type>[^;]+);[ \t]*name=\"(?<name>.+)\"[\r\n]+"
    let private endRx = Regex "[\r\n]*---.*Boundary.*[\r\n]+"

    let parseFormData(msg : string) =
        let mutable m = bounaryRx.Match msg
        let mutable startIndex = -1
        let mutable result = Map.empty
        let mutable currentName = null

        while m.Success do
            if startIndex >= 0 then
                let value = msg.Substring(startIndex, m.Index - startIndex)
                result <- Map.add currentName value result

            currentName <- m.Groups.["name"].Value
            startIndex <- m.Index + m.Length

            m <- m.NextMatch()

        if startIndex >= 0 then
            let m = endRx.Match(msg, startIndex)
            if m.Success then
                let value = msg.Substring(startIndex, m.Index - startIndex)
                result <- Map.add currentName value result
  

        result


    let private compile (code : string) (composition : string) =
        printfn "initializing FSCC"
        FSCC.repl <- true
        let code = code.Replace("\t", "    ")

        let temp = Path.GetTempFileName() + ".fs"
        File.WriteAllText(temp, code)
        let shaderNames = composition.Split(' ')
                
        printfn "starting FSCC"
        let ms = new System.IO.MemoryStream()
        let o = new System.IO.StreamWriter(ms)
        //let savedOut = System.Console.Out
        //System.Console.SetOut(o)

        let c = { FSCC.Config.inputFiles = []; FSCC.Config.language = FSCC.Language.GLSL; FSCC.Config.output = FSCC.StreamOut o; FSCC.Config.shaderNames = shaderNames |> Array.toList }

        FSCC.compile c code composition

        o.Flush()
        ms.ToArray()

    open System
    let appDomains = Array.init 4 (fun i -> DateTime.Now,AppDomain.CreateDomain(sprintf "FSCC%d" i))
    let rand = System.Random()

    let runInRandomDomain (f : unit -> string) =
        let index = rand.Next(appDomains.Length)
        let (t,d) = appDomains.[index]
        let name = d.FriendlyName

        let age = DateTime.Now - t
        let d =
            if age.TotalMinutes > 30.0 then
                printfn "killed AppDomain: %s" name
                AppDomain.Unload(d)
                let d = AppDomain.CreateDomain(name)
                appDomains.[index] <- (DateTime.Now, d)
                d
            else
                d


        let mine = System.AppDomain.CurrentDomain
        d.DoCallBack(System.CrossAppDomainDelegate(fun () ->
            let str = f()
            mine.SetData(System.AppDomain.CurrentDomain.FriendlyName, str)
        ))
        let result = mine.GetData(name) |> unbox<string>
        result

    let worker(ctx : HttpListenerContext) =
        Task.Factory.StartNew(fun () ->
            try
                printfn "%d workers running" (Interlocked.Increment(&workersRunning))
                while true do 
                    use reader = new StreamReader(ctx.Request.InputStream, ctx.Request.ContentEncoding)
                    let msg = reader.ReadToEnd()

                    if msg.Length <> 0 then
                        let data = parseFormData msg

                        match Map.tryFind "code" data, Map.tryFind "comp" data with
                            | Some code, Some comp ->
                                printfn "starting compiler"
                                let b = runInRandomDomain (fun () -> compile code comp |> System.Text.ASCIIEncoding.Default.GetString) |> System.Text.ASCIIEncoding.Default.GetBytes

                                printfn "reply"
                                ctx.Response.StatusCode <- 200
                                ctx.Response.ContentLength64 <- int64 b.Length
                                ctx.Response.OutputStream.Write(b, 0, b.Length)
                                ctx.Response.OutputStream.Close()
                            | _ ->
                                raise <| System.OperationCanceledException()
                    else
                        raise <| System.OperationCanceledException()
            with 
                | :? System.OperationCanceledException as e -> 
                    Interlocked.Decrement(&workersRunning) |> ignore
                | e ->
                    //printfn "ERROR: %A" e
                    Interlocked.Decrement(&workersRunning) |> ignore

        ) |> ignore

    let run() =
        listener.Start()
        listener.Prefixes.Add("http://*:8080/")
        
        
        while true do
            let ctx = listener.GetContext()
           
            ctx.Response.AddHeader("Access-Control-Allow-Origin", "*")
            worker(ctx)

    

module NewService =
    open System
    open System.Net
    open System.Collections.Concurrent
    open Nessos.FsPickler.Json
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.Interactive
    open Microsoft.FSharp.Compiler.Interactive.Shell
    open FShade.Compiler
    open FShade

    let pickler = FsPickler.CreateJson(true, true)
    let log fmt = Printf.kprintf (fun str -> Console.WriteLine("FSCC# {0}", str) ) fmt

    module Multipart =
        open System.Text.RegularExpressions

        let private bounaryRx = Regex "[\r\n]*---.*Boundary.*[\r\n]+Content-Disposition:[ \t]*(?<type>[^;]+);[ \t]*name=\"(?<name>.+)\"[\r\n]+"
        let private endRx = Regex "[\r\n]*---.*Boundary.*[\r\n]+"

        let parse(msg : string) =
            let mutable m = bounaryRx.Match msg
            let mutable startIndex = -1
            let mutable result = Map.empty
            let mutable currentName = null

            while m.Success do
                if startIndex >= 0 then
                    let value = msg.Substring(startIndex, m.Index - startIndex)
                    result <- Map.add currentName value result

                currentName <- m.Groups.["name"].Value
                startIndex <- m.Index + m.Length

                m <- m.NextMatch()

            if startIndex >= 0 then
                let m = endRx.Match(msg, startIndex)
                if m.Success then
                    let value = msg.Substring(startIndex, m.Index - startIndex)
                    result <- Map.add currentName value result
  

            result

    type CompilationRequest = { session : Guid; code : string; composition : list<string> }
    type CompletionRequest = { session : Guid; code : string; line : int; col : int }
    type CompilationResult = { success : bool; compiledCode : string; error : string; compilerErrors : list<CompilerError> }

    module Compilation =
        open System.Threading
        open System.Threading.Tasks
        open System.Security.Policy
        open System.Security
        open System.Security.Permissions

        let toEffectMethod = FShade.Compiler.ReflectionExtensions.getMethodInfo <@ toEffect @>
        let buildFunctionMethod = FShade.Compiler.ReflectionExtensions.getMethodInfo <@ Aardvark.Base.FunctionReflection.FunctionBuilder.BuildFunction<int, int>(null, null) @>


        type SessionProxy() =
            inherit MarshalByRefObject()

            static let simpleCode = """
                module Simple =
                    open Aardvark.Base
                    open FShade
    
                    type V = { [<Semantic("Positions")>] p : V4d }

                    let trafo(v : V) =
                        vertex {
                            return { p = 2.0 * v.p }
                        }

                    let shade(v : V) =
                        fragment {
                            return V4d.IIII
                        }
                    """

            let s = FSCC.Service.Fsi.Session()

            do
                s.execute ("#I @\"" + System.Environment.CurrentDirectory + "\"") |> ignore
                s.addReference "Aardvark.Base.dll"
                s.addReference "Aardvark.Base.TypeProviders.dll"
                s.addReference "Aardvark.Base.FSharp.dll"
                s.addReference "FShade.Compiler.dll"
                s.addReference "FShade.dll"

                s.compileModule simpleCode |> ignore


            member x.completions (c : CompletionRequest) =
                s.Completions(c.code, c.line, c.col)

            member x.compile (r : CompilationRequest) =
                let code = "[<ReflectedDefinition>]\r\n" + r.code

                match s.compileModule code with
                    | FsiSuccess t ->
                    
                        let methods = r.composition |> List.map (fun n -> t.GetMethod n)
                        if methods |> List.forall (fun o -> o <> null) then

                            let effects = 
                                methods |> List.map (fun mi ->
                                    let p = mi.GetParameters().[0]
                                    let toEffect = toEffectMethod.MakeGenericMethod [|p.ParameterType; mi.ReturnType.GetGenericArguments().[0]|]
                                    let build = buildFunctionMethod.MakeGenericMethod [|p.ParameterType; mi.ReturnType|]

                                    let f = build.Invoke(null, [|null; mi|])

                                    let effect = toEffect.Invoke(null, [|f|]) |> unbox<Compiled<Effect, ShaderState>>

                                    effect
                                )

                            let e = compose effects

                            match GLES.compileEffect e with
                                | Aardvark.Base.Prelude.Success(uniforms, code) ->

                                    let samplerStates = uniforms |> Map.toList |> List.filter (fun (s,v) -> v.IsSamplerUniform) |> List.map (fun (s,v) ->
                                                            let sem, sam = v.Value |> unbox<string * SamplerState>

                                                            let code = GlslSamplers.compileSamplerState s sam

                                                            let code = FShade.Utils.String.linePrefix "//" code

                                                            sprintf "//string %s = \"%s\"\r\n%s" s sem code
                                                        ) |> String.concat "\r\n"

                                    { success = true; compiledCode = sprintf "%s\r\n//SAMPLERSTATES\r\n%s" code samplerStates; error = ""; compilerErrors = [] }

                                | Aardvark.Base.Prelude.Error e ->
                                    { success = false; compiledCode = ""; error = e; compilerErrors = [] }

                        else
                            let notFound = r.composition |> List.filter (fun n -> t.GetMethod n = null)
                            { success = false; compiledCode = ""; error = sprintf "could not find shader functions for: %A" notFound; compilerErrors = [] }



                    | FsiError err ->
                        { success = false; compiledCode = ""; error = "compiler error"; compilerErrors = err.errors |> List.map (fun e -> { e with line = e.line - 1}) }


        type Session() =
            static let mutable currentId = 0
            static let mutable domainId = 0
            let l = obj()

            let id = Interlocked.Increment(&currentId)
            let mutable domain = AppDomain.CurrentDomain
            let mutable created = DateTime.Now
            let mutable s = Unchecked.defaultof<SessionProxy>
            let mutable servedRequests = 0
            let mutable isResetting = 0

            let newDomain() =
                let id = Interlocked.Increment(&domainId)
                let name = sprintf "FSI%d" id


////                let ev = Evidence()
////                ev.AddHostEvidence(new Zone(SecurityZone.MyComputer));
////                let grantSet = SecurityManager.GetStandardSandbox(ev)

////                let grantSet = PermissionSet(PermissionState.None)
////                grantSet.AddPermission(SecurityPermission(SecurityPermissionFlag.Execution)) |> ignore

                let dom = AppDomain.CreateDomain(name)

                //let dom = AppDomain.CreateDomain(name)
                dom

            let init() =
                let dom = newDomain()
                
                let t = typeof<SessionProxy>
                let s = dom.CreateInstanceAndUnwrap(t.Assembly.FullName, t.FullName) |> unbox<SessionProxy>

                dom, s

            do 
                let dom, session = init()
                domain <- dom
                s <- session
                created <- DateTime.Now
                servedRequests <- 0

            member x.Id = id

            member x.Reset() =
                let o = Interlocked.Exchange(&isResetting, 1)
                if o = 0 then
                    log "attempting to reset session: %A" id
                    let newDomain, newSession = init()

                    lock l (fun () ->
                        //s.kill()
                        AppDomain.Unload(domain)
                        System.GC.Collect()
                        //s.execute "#quit;;" |> ignore
                        domain <- newDomain
                        s <- newSession
                        created <- DateTime.Now
                        servedRequests <- 0
                        Interlocked.Exchange(&isResetting, 0) |> ignore
                    )
                    log "session resetted: %A" id

            member x.compile (request : CompilationRequest) =
                lock l (fun () ->
                    let res = s.compile request
                    servedRequests <- servedRequests + 1
                    res
                )

            member x.completions (request : CompletionRequest) =
                //lock l (fun () ->
                    s.completions request
                //)

            member x.CreationTime = created

            member x.RequestCount = servedRequests

        module Async =
            let Delay(ms : int) =
                Task.Delay(ms).ContinueWith (Func<Task,unit>(fun (t : Task) -> ())) |> Async.AwaitTask

        let private allSessions = ConcurrentHashSet<Session>()
        let private sessionPool = ConcurrentBag<Session>()
        let private sessionSem = new SemaphoreSlim(0)

        let createSession() =
            let s = Session()
            allSessions.Add s |> ignore
            s

        let init() = 
            log "creating default sessions"
            for i in 0..1 do 
                let s = createSession()
                sessionPool.Add s
                sessionSem.Release() |> ignore
            log "sessions created"

            async {
                try
                    while true do
                        do! Async.Delay(1000)

                        for s in allSessions do
                            let age = DateTime.Now - s.CreationTime
                            if s.RequestCount > 2 && age.TotalSeconds > 20.0 then
                                
                                s.Reset()

                        ()
                with
                    | :? OperationCanceledException -> ()
                    | e -> log "timer faulted: %A" e

            } |> Async.StartAsTask |> ignore
            


        let getSession (id : Guid) =
            sessionSem.Wait()
            match sessionPool.TryTake() with
                | (true, s) -> 
                    s
                | _ ->
                    failwith "impossible"
                    
        let releaseSession (s : Session) =
            sessionPool.Add s
            sessionSem.Release()

        let compile (r : CompilationRequest) =
            let s = getSession r.session
            try
               s.compile r
            finally
                releaseSession s |> ignore

        let completions (r : CompletionRequest) =
            let s = getSession r.session
            try
               s.completions r
            finally
                releaseSession s |> ignore


    let tryReadRequest (str : string) =
        let data = Multipart.parse str

        match Map.tryFind "session" data, Map.tryFind "code" data, Map.tryFind "comp" data with
            | Some session, Some code, Some comp ->
                let code = code.Replace("\t", "    ")
                let composition = comp.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

                match Guid.TryParse session with
                    | (true, guid) ->
                        Some { session = guid; code = code; composition = composition |> Array.toList }
                    | _ ->
                        None
            | _ -> None

    let tryReadCompletionRequest (str : string) =
        let data = Multipart.parse str

        match Map.tryFind "session" data, Map.tryFind "code" data, Map.tryFind "line" data, Map.tryFind "col" data with
            | Some session, Some code, Some l, Some c ->
                let code = code.Replace("\t", "    ")
                let l = Int32.Parse l
                let c = Int32.Parse c

                match Guid.TryParse session with
                    | (true, guid) ->
                        Some { session = guid; code = code; line = l; col = c }
                    | _ ->
                        None
            | _ -> None

    let processRequestAsync (ctx : HttpListenerContext) =
        async {
            let request = ctx.Request
            let response = ctx.Response
            use r = new System.IO.StreamReader(request.InputStream)
            use w = new System.IO.StreamWriter(response.OutputStream)

            let fail (code : HttpStatusCode) fmt = 
                Printf.kprintf (fun str -> w.WriteLine("<html><body><h1>{0}</h1></body></html>", str); response.StatusCode <- int code; log "%s" str) fmt

            if request.HttpMethod = "POST" then
                let! input = r.ReadToEndAsync() |> Async.AwaitTask

                match tryReadRequest input with
                    | Some request ->
                        log "request from: %A" request.session

                        let result = 
                            try
                                Compilation.compile request
                            with e ->
                                { success = false; compiledCode = ""; error = e.ToString(); compilerErrors = [] }
                                
                        let str = pickler.PickleToString(result)
                        w.WriteLine(str)
                        response.StatusCode <- int HttpStatusCode.OK


                    | None ->
                        match tryReadCompletionRequest input with
                            | Some request ->
                                log "request from: %A" request.session

                                let result = 
                                    try
                                        Compilation.completions request
                                    with e ->
                                        []
                                
                                let str = pickler.PickleToString(result)
                                w.WriteLine(str)
                                response.StatusCode <- int HttpStatusCode.OK
                            | None ->
                                fail HttpStatusCode.BadRequest "could not parse request"

                ()

            else
                fail HttpStatusCode.MethodNotAllowed "unexpected HTTP method: %A" request.HttpMethod

            w.Flush()
            response.KeepAlive <- true
            response.Close()
        }

    let runAsync() =
        Compilation.init()
        let l = new HttpListener()
        l.Start()
        l.Prefixes.Clear()
        l.Prefixes.Add("http://*:8080/")
        
        async {
            try
                while true do
                    let! ctx = l.GetContextAsync() |> Async.AwaitTask
                    if not <| ctx.Request.RawUrl.EndsWith ".ico" then
                        ctx.Response.AddHeader("Access-Control-Allow-Origin", "*")

                        processRequestAsync ctx |> Async.StartAsTask |> ignore

                    else
                        ctx.Response.StatusCode <- int HttpStatusCode.NotFound
                        ctx.Response.Close()

            with 
                | :? OperationCanceledException -> 
                    log "shutdown"
                | e ->
                    log "unexpected error: %A" e
        }

    let run() =
        runAsync() |> Async.RunSynchronously


[<EntryPoint>]
let main argv = 
    
    NewService.run()
    printfn "%A" argv
    0 // return an integer exit code
