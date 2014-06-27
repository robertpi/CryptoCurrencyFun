namespace CryptoCurrencyFun
open System
open System.Collections.Concurrent
open System.Net
open CryptoCurrencyFun.Messages

type NewTransactionEventArgs(trans: Transaction, locations: IPAddress[]) =
    inherit EventArgs()
    member x.Transaction = trans
    member x.Locations = locations

type NewTransactionLocationEventArgs(transHash: string, location: IPAddress) =
    inherit EventArgs()
    member x.TransactionHash = transHash
    member x.Location = location

// TODO question, do we care about where the new block came from??
type NewBlockEventArgs(block: Block) =
    inherit EventArgs()
    member x.Block = block


type internal TransactionDetails = 
    { SearchCount: int
      Transaction: Option<Transaction>
      Locations: list<IPAddress> }
    static member Empty =
        { SearchCount = 0
          Transaction = None
          Locations = [] }

// TODO really needs a place to store the block chain to work properly

type MemoryPool(conn: IPeerToPeerConnectionManager) =
    let maxConcurrentSearches = 5

    let transations = new ConcurrentDictionary<string, TransactionDetails>()
    let blockSearches = new ConcurrentDictionary<string, list<IPAddress>>()

    let newTransaction = new Event<_>()
    let newTransactionLocation = new Event<_>()
    let newBlock = new Event<_>()

    let getInventoryDetails address iv =
        conn.SendTo address (GetData (InventoryDetails.Create 1uL [| iv |]))

    let insertTrans (iv: InventoryVector) address =
        // TODO need to check wether a transaction is part of a known block before inserting, 
        // but this requires way to store blocks

        newTransactionLocation.Trigger (new NewTransactionLocationEventArgs(iv.HashAsString, address))

        let initDetails = { TransactionDetails.Empty with Locations = [address]; SearchCount = 1 }
        let details = transations.GetOrAdd(iv.HashAsString, initDetails)
        let getTransdetails() = getInventoryDetails address iv
        if initDetails = details then
            // we're good start search
            getTransdetails()
        else
            let newDetails = 
                match details.Transaction with
                | None when details.SearchCount <= maxConcurrentSearches ->
                    getTransdetails()
                    { details with Locations = address :: details.Locations; SearchCount = details.SearchCount + 1 }
                | _ ->
                    { details with Locations = address :: details.Locations }
            let rec addLoop newDetails oldDetails =
                if not (transations.TryUpdate(iv.HashAsString, newDetails, oldDetails)) then
                    let currentDetails = transations.[iv.HashAsString]
                    let nextDetails = { currentDetails with Locations = address :: details.Locations }
                    addLoop nextDetails currentDetails 
            addLoop newDetails details

    let getInvBlock (iv: InventoryVector) address =
        let res, addresses = blockSearches.TryGetValue iv.HashAsString
        if not res || res && addresses.Length < maxConcurrentSearches then
            getInventoryDetails address iv
            let oldDetails = if res then addresses else []
            let newDetails = address :: oldDetails
            let rec addLoop newDetails oldDetails =
                if not (blockSearches.TryUpdate(iv.HashAsString, newDetails, oldDetails)) then
                    let currentDetails = blockSearches.[iv.HashAsString]
                    let nextDetails = address :: currentDetails
                    addLoop nextDetails currentDetails 
            addLoop newDetails oldDetails

    let blockFound (block: Block) =
        // TODO again we really need to check block chain to see if it's new ...
        newBlock.Trigger (new NewBlockEventArgs(block))
        for trans in block.Transactions do
            transations.TryRemove trans.TransactionHashAsString |> ignore


    let handleMessage  (ea: MessageReceivedEventArgs) =
        match ea.Message with
        | Inv inv ->
            inv.Invertory 
            |> Seq.filter (fun x -> x.Type = InventoryVectorType.MsgTx)
            |> Seq.iter (fun x -> insertTrans x ea.Address)

            inv.Invertory 
            |> Seq.filter (fun x -> x.Type = InventoryVectorType.MsgBlock)
            |> Seq.iter (fun x -> getInvBlock x ea.Address)
        | Tx trans ->
            let details = transations.[trans.TransactionHashAsString]
            if Option.isNone details.Transaction then
                newTransaction.Trigger (new NewTransactionEventArgs(trans, Array.ofList details.Locations))
                let newDetails = { details with Transaction = Some trans }
                let rec addLoop newDetails oldDetails =
                    if not (transations.TryUpdate(trans.TransactionHashAsString, newDetails, oldDetails)) then
                        let currentDetails = transations.[trans.TransactionHashAsString]
                        if Option.isNone currentDetails.Transaction then
                            let nextDetails = { currentDetails with Transaction = Some trans }
                            addLoop nextDetails currentDetails 
                addLoop newDetails details
        | Block block ->
            blockFound block 
        | _ -> ()

    do conn.MessageReceived |> Event.add handleMessage

    [<CLIEvent>]
    member x.NewTransaction = newTransaction.Publish
    [<CLIEvent>]
    member x.NewTransactionLocation = newTransactionLocation.Publish
    [<CLIEvent>]
    member x.NewBlock = newBlock.Publish
