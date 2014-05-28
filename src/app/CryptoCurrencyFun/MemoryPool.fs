namespace CryptoCurrencyFun
open System.Collections.Concurrent
open System.Collections.Generic
open System.Net
open CryptoCurrencyFun.Messages
// TODO member pool must
// - store transactions as they're found
// - store the locations we've seen transactiosn from
// - genreate a new event for a transactions
// - when a block is seen it must clear all assoicated transactions
// - generate an even to notify that transactions have been cleared

type internal TransactionDetails = 
    { Transaction: Transaction
      Locations: ConcurrentBag<IPAddress> }

type MemoryPool(conn: PeerToPeerConnectionManager) =
    let transations = new ConcurrentDictionary<string, Transaction>()

    let newTransaction = new Event<_>()
    let newTransactionLocation = new Event<_>()
    let transationsCommitedToBlockchain = new Event<_>()

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


    member x.DoStuff() = ()