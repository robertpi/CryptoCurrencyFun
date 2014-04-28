open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent
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

// --------------------------------


let printMessageDetails (ea: MessageReceivedEventArgs) =
    printfn "Message received %s from %O" ea.Message.MessageNameText ea.Address

// --------------------------------


let statsOnMessageType (event: IEvent<Handler<MessageReceivedEventArgs>,MessageReceivedEventArgs>) = 
    event
    |> Event.map (fun ea -> [ea.Message.MessageNameText])
    |> Event.histogram
    |> Event.every 50
    |> Event.add (printfn "%40A")

// --------------------------------

let printInvNode (inv: InventoryVector) =
    let hash = Conversion.littleEndianBytesToHexString inv.Hash
    sprintf "%s - %O" hash inv.Type

let invNodeToHashes (ea: MessageReceivedEventArgs) =
    match ea.Message with
    | Inv inv ->
        let items =
            inv.Invertory 
            |> Seq.map (printInvNode)
            |> Seq.toList
        Some items
    | _ -> None

let whatInv (event: IEvent<Handler<MessageReceivedEventArgs>,MessageReceivedEventArgs>) = 
    event
    |> Event.choose invNodeToHashes
    |> Event.histogram
    |> Event.every 50
    |> Event.add (printfn "%40A")

// --------------------------------

let insertTrans (invAddress: Dictionary<string, list<IPAddress>>) hash address =
    let hash = Conversion.littleEndianBytesToHexString hash
    if invAddress.ContainsKey(hash) then
        invAddress.[hash] <- address :: invAddress.[hash]
    else
        invAddress.Add(hash, [address])

let invNodeToDetails (invAddress: Dictionary<string, list<IPAddress>>) (ea: MessageReceivedEventArgs) =
    match ea.Message with
    | Inv inv ->
        let items =
            inv.Invertory 
            |> Seq.filter (fun x -> x.Type = InventoryVectorType.MsgTx)
            |> Seq.iter (fun x -> insertTrans invAddress x.Hash ea.Address)
        invAddress
    | _ -> invAddress

let transations = new ConcurrentDictionary<string, Transaction>()

let getData (connMan: PeerToPeerConnectionManager) (invAddress: Dictionary<string, list<IPAddress>>) =
    let candidates = 
        invAddress 
        |> Seq.filter (fun x -> not (transations.ContainsKey x.Key))
        |> Seq.sortBy (fun x -> -x.Value.Length)
        //|> Seq.take 2
        |> Seq.map (fun x -> List.head x.Value,  InventoryVector.Create InventoryVectorType.MsgTx (Conversion.hexStringToBytes x.Key |> Array.rev))
        |> Seq.groupBy fst
        |> Seq.map (fun (ip, commonInvs) -> 
                                    let invVects = Seq.map snd commonInvs |> Seq.toArray
                                    ip, InventoryDetails.Create (uint64 invVects.Length)  invVects)
    for (address, invVec) in candidates do
        connMan.SendTo address (GetData invVec) 


let testGetData (connMan: PeerToPeerConnectionManager) (event: IEvent<Handler<MessageReceivedEventArgs>,MessageReceivedEventArgs>)  = 
    event
    |> Event.scan invNodeToDetails (new Dictionary<string, list<IPAddress>>())
    |> Event.every 50
    |> Event.add (getData connMan)

let handleTransMessage (ea: MessageReceivedEventArgs) =
    match ea.Message with
    | Tx trans ->
        let hash = Conversion.littleEndianBytesToHexString trans.TransactionHash
        printfn "Got tx %s" hash
        transations.TryAdd(hash, trans) |> ignore
    | _ -> ()

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

    let seedIps = SeedDns.Litecoin |> Seq.collect nslookup 
    let connMan = new PeerToPeerConnectionManager(MagicNumbers.Litecoin, Ports.Litecoin, seedIps)

    //connMan.MessageReceived |> statsOnMessageType

    connMan.MessageReceived |> (testGetData connMan)
    connMan.MessageReceived |> Event.add handleTransMessage

    connMan.Connect()

    System.Console.ReadLine() |> ignore

    //connMan.Broadcast(MemPool)
    //System.Console.ReadLine() |> ignore

    0