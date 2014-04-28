namespace BitcoinFs.Messages

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
