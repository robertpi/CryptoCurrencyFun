namespace BitcoinFs.Messages
open BitcoinFs
open System
open BitcoinFs.CommonMessages

type Version106 =
    { AddressFrom: NetworkAddress
      Nonce: uint64
      UserAgent: string
      StartHeight: int
      Relay: bool }

type Version =
    { Version: int
      Service: uint64
      Timestamp: int64
      AddressReceive: NetworkAddress
      Extras106: option<Version106> }

type Address = //addr 
    { Count: int64
      AddressList: NetworkAddress[] }

type InventoryDetails = // inv, getdata & notfound (at the moment don't see how getdata is useful)
    { Count: int64
      Invertory: InventoryVector[] }

type GetSpec = // getblocks , getheaders
    { Version: uint32
      HashCount: uint64
      BlockLocatorHashes: byte[][]
      HashStop: byte[] }

type Headers = // headers
    { Count: uint32
      Headers: Header[] }

type PingPong = // ping pong
    { Nonce: uint32 }

type Alert =
    { Version: int32
      RelayUntil: int64
      Expiration: int64
      ID: int32
      Cancel: int32
      SetCancel: int32[]
      MinVer: int32
      MaxVer: int32
      SetSubVer: string[]
      Priority: int32
      Comment: string
      StatusBar: string
      Reserved: string }


type Output =
    { Value: int64
      OutputScriptLength: int64
      OutputScript: array<byte>
      ParsedOutputScript: option<array<Op>>
      CanonicalOutputScript: option<CanonicalOutputScript> }

type Input =
    { InputHash: array<byte>
      InputTransactionIndex: int
      ResponseScriptLength: int64
      ResponseScript: array<byte>
      ParsedResponseScript: option<array<Op>>
      SequenceNumber: int }

type Transaction = 
    { TransactionVersion: int
      NumberOfInputs: int64
      Inputs: array<Input>
      NumberOfOutputs: int64
      Outputs: array<Output>
      LockTime: int
      TransactionHash: array<byte> }

type Block =
    { OffSet: int64
      Length: int
      Version: int
      Hash: array<byte>
      MerKleRoot: array<byte>
      Timestamp: DateTime
      Target: int
      Nonce: int
      NumberOfTransactions: int64
      Transactions: array<Transaction> }
