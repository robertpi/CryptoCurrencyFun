open System
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent
open System.Net
open System.Text.RegularExpressions
open CryptoCurrencyFun
open CryptoCurrencyFun.Constants
open CryptoCurrencyFun.Messages
open NLog
open NLog.Layouts
open NLog.Targets
open NLog.Config

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

    AppDomain.CurrentDomain.UnhandledException 
    |> Event.add (fun x -> printfn "Got exception: %O" x.ExceptionObject)

    let config = new LoggingConfiguration()

    let layout = new SimpleLayout(@"${date:yyyy--gMM-ddTHH\:mm\:ss} ${logger} ${message}")

    let consoleTarget = new ColoredConsoleTarget()
    config.AddTarget("console", consoleTarget)
    consoleTarget.Layout <- layout

    let consoleRule = new LoggingRule("*", LogLevel.Debug, consoleTarget)
    config.LoggingRules.Add(consoleRule)

    LogManager.Configuration <- config

    let connDetails = 
        { NetworkDetails.Bitcoin with
            DnsResolution = Connection.shellNslookup Connection.resolvedAddressRegex }

    let connMan = new PeerToPeerConnectionManager(connDetails, 100)

    //connMan.MessageReceived |> statsOnMessageType

    //connMan.MessageReceived |> (testGetData connMan)
    //connMan.MessageReceived |> Event.add handleTransMessage

    let memPool = new MemoryPool(connMan)

    memPool.NewTransaction |> Event.add (fun ea -> printfn "New transaction: %A" ea.Transaction)
    //memPool.NewTransactionLocation |> Event.add (fun ea -> printfn "New transaction location: %O - %s" ea.Location ea.TransactionHash)
    memPool.NewBlock |> Event.add (fun ea -> printfn "New block: %A" ea.Block)

    connMan.Connect()

    System.Console.ReadLine() |> ignore

    //connMan.Broadcast(MemPool)
    //System.Console.ReadLine() |> ignore

    0