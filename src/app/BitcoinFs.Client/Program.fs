open System.Diagnostics
open System.Net
open System.Text.RegularExpressions
open BitcoinFs
open BitcoinFs.Constants
open NLog
open NLog.Layouts
open NLog.Targets
open NLog.Config

let reduceSeedHostList = [ "seed.bitcoin.sipa.be" ]

let addressRegex = new Regex("Address: (.+)")

let nslookup dns =
    let nsProd = Process.Start(new ProcessStartInfo ("nslookup", dns, UseShellExecute = false, RedirectStandardOutput = true))
    let text = nsProd.StandardOutput.ReadToEnd()
    let matches: seq<Match> = addressRegex.Matches text |> Seq.cast
    matches
    |> Seq.map (fun m -> IPAddress.Parse m.Groups.[1].Value)

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

    let seedIps = SeedDns.Bitcoin |> Seq.collect nslookup 
    let connMan = new PeerToPeerConnectionManager(MagicNumbers.Bitcoin, Ports.Bitcoin, seedIps)
    connMan.Connect()

    System.Console.ReadLine() |> ignore

    //connMan.BroadcastMemPool()
    //System.Console.ReadLine() |> ignore

    0