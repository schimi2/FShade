#if INTERACTIVE
#else
namespace Logging
#endif

open System


type Severity =
    | Debug     = 1
    | Info      = 2
    | Message   = 3
    | Warning   = 4
    | Error     = 5
    | Critical  = 6



type ILogger =
    abstract member Log : int -> Severity -> string -> unit


module String =
    open System.Text
    open System.Text.RegularExpressions
    let newLine = Regex @"\r?\n"
    
    let lines (str : string) =
        newLine.Split str |> Array.toList

    let indent (level : int) (str : string) =
        let lines = newLine.Split str
        let spaces = String(' ', level * 4)
        let b = StringBuilder()
        for l in lines do
            b.AppendFormat("    {0}", l) |> ignore

        b.ToString()

type ConsoleLogger() =
    static let defaultForeground = Console.ForegroundColor
    static let defaultBackground = Console.BackgroundColor

    static let colors =
        Map.ofList [
            Severity.Debug,      (ConsoleColor.DarkGray, defaultBackground)
            Severity.Info,       (defaultForeground, defaultBackground)
            Severity.Message,    (ConsoleColor.DarkGreen, defaultBackground)
            Severity.Warning,    (ConsoleColor.Yellow, defaultBackground)
            Severity.Error,      (ConsoleColor.Red, defaultBackground)
            Severity.Critical,   (ConsoleColor.White, ConsoleColor.Red)
        ]

    static do Console.SetBufferSize(Console.BufferWidth, int Int16.MaxValue - 1)


    
    let prefix = 
        new System.Threading.ThreadLocal<string>(fun () -> sprintf "t%d# " System.Threading.Thread.CurrentThread.ManagedThreadId)

    member x.Log (indent : int) (v : Severity) (message : string) =
        let (f,b) = Map.find v colors

        let fo = Console.ForegroundColor
        let bo = Console.BackgroundColor

        let lines = String.lines message
        let spaces = String(' ', indent * 2)
        for l in lines do
            Console.ForegroundColor <- fo
            Console.BackgroundColor <- bo
            Console.Write("{0}{1}", prefix.Value, spaces)
            Console.ForegroundColor <- f
            Console.BackgroundColor <- b
            Console.WriteLine("{0}", l)

        Console.ForegroundColor <- fo
        Console.BackgroundColor <- bo

    interface ILogger with member x.Log i v m = x.Log i v m


module Logger =
    open System.Threading

    let logger = ConsoleLogger()

    let indent = new ThreadLocal<int>(fun () -> 0)


    let info fmt = Printf.kprintf (fun str -> logger.Log indent.Value Severity.Info str) fmt
    let debug fmt = Printf.kprintf (fun str -> logger.Log indent.Value Severity.Debug str) fmt
    let message fmt = Printf.kprintf (fun str -> logger.Log indent.Value Severity.Message str) fmt
    let warn fmt = Printf.kprintf (fun str -> logger.Log indent.Value Severity.Warning str) fmt
    let error fmt = Printf.kprintf (fun str -> logger.Log indent.Value Severity.Error str) fmt
    let critical fmt = Printf.kprintf (fun str -> logger.Log indent.Value Severity.Critical str) fmt

    let region str f =
        try
            info "%s" str
            indent.Value <- indent.Value + 1
            f()
        finally
            indent.Value <- indent.Value - 1

    let timed str f =
        let sw = System.Diagnostics.Stopwatch()
        
        try
            info "%s" str
            indent.Value <- indent.Value + 1
            sw.Start()
            f()
        finally
            sw.Stop()
            indent.Value <- indent.Value - 1
            
            info "%s (%fms)" str sw.Elapsed.TotalMilliseconds

module TestLogging =
    let run() =
        Logger.timed "something" (fun () ->
            Logger.debug "debug"
            Logger.info "info"
            Logger.message "message"
            Logger.warn "warn"
            Logger.error "error"
            Logger.critical "critical"
        )