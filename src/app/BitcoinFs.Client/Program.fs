open System.Diagnostics
open System.Net
open System.Text.RegularExpressions
open BitcoinFs
open BitcoinFs.Constants
open BitcoinFs.Messages
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

let printMessageDetails (ea: MessageReceivedEventArgs) =
    printfn "Message received %s from %O" ea.Message.MessageNameText ea.Address

let statsOnMessageType (event: IEvent<Handler<MessageReceivedEventArgs>,MessageReceivedEventArgs>) = 
    event
    |> Event.map (fun ea -> [ea.Message.MessageNameText])
    |> Event.histogram
    |> Event.every 50
    |> Event.add (printfn "%40A")

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

    connMan.MessageReceived |> statsOnMessageType

    connMan.Connect()

    System.Console.ReadLine() |> ignore

    connMan.Broadcast(MemPool)
    System.Console.ReadLine() |> ignore

    0