namespace BitcoinFs.CommonMessages
type RawMessage =
    { Magic: uint32
      Command: string
      Length: uint32
      Checksum: uint32
      Message: byte[] }


type NetworkAddress = 
    { Timestamp: option<uint32>
      Service: uint64
      Address: byte[]
      Port: uint16 }

type InventoryVectorType =
    | Error = 0
    | MsgTx = 1
    | MsgBlock = 2

type InventoryVector =
    { Type: InventoryVectorType
      Hash: byte[] }

type Header =
    { Version: uint32
      PrevBlock: byte[]
      MerkleRoot: byte[]
      Timestamp: uint32
      Bits: uint32
      Nonce: uint32
      TransactionCount: uint64 }
