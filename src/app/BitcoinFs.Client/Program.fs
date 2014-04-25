open BitcoinFs
open BitcoinFs.Constants
open NLog
open NLog.Layouts
open NLog.Targets
open NLog.Config

let reduceSeedHostList = [ "seed.bitcoin.sipa.be" ]

[<EntryPoint>]
let main argv =
    let config = new LoggingConfiguration()

    let layout = new SimpleLayout(@"${date:yyyy--gMM-ddTHH\:mm\:ss} ${logger} ${message}")

    let consoleTarget = new ColoredConsoleTarget()
    config.AddTarget("console", consoleTarget)
    consoleTarget.Layout <- layout

    let consoleRule = new LoggingRule("*", LogLevel.Debug, consoleTarget)
    config.LoggingRules.Add(consoleRule)

    LogManager.Configuration <- config
    let connMan = new PeerToPeerConnectionManager(Ports.Bitcoin, SeedDns.Bitcoin, MagicNumbers.Bitcoin)
    connMan.Connect()

    System.Console.ReadLine() |> ignore

    connMan.BroadcastPing()

    System.Console.ReadLine() |> ignore

    0