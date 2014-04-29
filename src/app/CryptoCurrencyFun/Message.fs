namespace CryptoCurrencyFun.Messages

type Message =
    | Version of Version
    | Verack
    | Addr of Address
    | Inv of InventoryDetails
    | GetData of InventoryDetails
    | NotFound of InventoryDetails
    | GetBlocks of GetSpec
    | GetHeaders of GetSpec
    | Tx of Transaction
    | Block of Block
    | Headers of Headers
    | GetAddr
    | MemPool
    | CheckOrder
    | SumbitOrder
    | Reply
    | Ping of PingPong
    | Pong of PingPong
    | FilterLoad
    | FilterAdd
    | FilterClear
    | MerkleBlock
    | Alert of Alert
    | Unknown of (string * byte[])
    member x.MessageNameText =
        match x with
        | Version _ -> MessageNames.Version
        | Verack -> MessageNames.Verack
        | Addr _ -> MessageNames.Addr
        | Inv _ -> MessageNames.Inv
        | GetData _ -> MessageNames.GetData
        | NotFound _ -> MessageNames.NotFound
        | GetBlocks _ -> MessageNames.GetBlocks
        | GetHeaders _ -> MessageNames.GetHeaders
        | Tx _ -> MessageNames.Tx
        | Block _ -> MessageNames.Block
        | Headers _ -> MessageNames.Headers
        | GetAddr -> MessageNames.GetAddr
        | MemPool -> MessageNames.MemPool
        | CheckOrder -> MessageNames.CheckOrder
        | SumbitOrder -> MessageNames.SumbitOrder
        | Reply -> MessageNames.Ping
        | Ping _ -> MessageNames.Ping
        | Pong _ -> MessageNames.Pong
        | FilterLoad -> MessageNames.FilterLoad
        | FilterAdd -> MessageNames.FilterAdd
        | FilterClear -> MessageNames.FilterClear
        | MerkleBlock -> MessageNames.MerkleBlock
        | Alert _ -> MessageNames.Alert
        | Unknown (name, _) -> name
    member x.Serialize() =
        match x with
        | Version v -> v.Serialize()
        | Verack -> [||]
        | Addr a -> a.Serialize()
        | Inv i -> i.Serialize()
        | GetData  gd -> gd.Serialize()
        | NotFound  nf -> nf.Serialize()
        | GetBlocks  gb -> gb.Serialize()
        | GetHeaders gh -> gh.Serialize()
        | Tx t -> t.Serialize()
        | Block b -> b.Serialize()
        | Headers h -> h.Serialize()
        | GetAddr -> [||]
        | MemPool -> [||]
        | CheckOrder -> [||]
        | SumbitOrder -> [||]
        | Reply -> [||]
        | Ping p -> p.Serialize()
        | Pong p -> p.Serialize()
        | FilterLoad -> [||]
        | FilterAdd -> [||]
        | FilterClear -> [||]
        | MerkleBlock -> [||]
        | Alert a -> a.Serialize()
        | Unknown (_, buffer) -> buffer
