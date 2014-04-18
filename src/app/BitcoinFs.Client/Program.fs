open BitcoinFs
open NLog
open NLog.Layouts
open NLog.Targets
open NLog.Config

let port = 8333
let seedHost = "bitseed.xf2.org"

[<EntryPoint>]
let main argv =
    let config = new LoggingConfiguration()

    let layout = new SimpleLayout(@"${date:yyyy-MM-ddTHH\:mm\:ss} ${logger} ${message}")

    let consoleTarget = new ColoredConsoleTarget()
    config.AddTarget("console", consoleTarget)
    consoleTarget.Layout <- layout

    let consoleRule = new LoggingRule("*", LogLevel.Debug, consoleTarget)
    config.LoggingRules.Add(consoleRule)

    LogManager.Configuration <- config
    let connMan = new PeerToPeerConnectionManager(port, seedHost)
    connMan.Connect()

    System.Console.ReadLine() |> ignore

    0