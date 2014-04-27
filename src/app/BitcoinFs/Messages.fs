namespace BitcoinFs.Messages
open BitcoinFs
open BitcoinFs.Constants
open System

module MessageNames =
    [<Literal>]
    let Version = "version"
    [<Literal>]
    let Verack = "verack"
    [<Literal>]
    let Addr = "addr"
    [<Literal>]
    let Inv = "inv"
    [<Literal>]
    let GetData = "getdata"
    [<Literal>]
    let NotFound = "notfound"
    [<Literal>]
    let GetBlocks = "getblocks"
    [<Literal>]
    let GetHeaders = "getheaders"
    [<Literal>]
    let Tx = "tx"
    [<Literal>]
    let Block = "block"
    [<Literal>]
    let Header = "header"
    [<Literal>]
    let GetAddr = "getaddr"
    [<Literal>]
    let MemPool = "mempool"
    [<Literal>]
    let CheckOrder = "checkorder"
    [<Literal>]
    let SumbitOrder = "submitorder"
    [<Literal>]
    let Reply = "reply"
    [<Literal>]
    let Ping = "ping"
    [<Literal>]
    let Pong = "pong"
    [<Literal>]
    let FilterLoad = "filterload"
    [<Literal>]
    let FilterAdd = "filteradd"
    [<Literal>]
    let FilterClear = "filterclear"
    [<Literal>]
    let MerkleBlock = "merkleblock"
    [<Literal>]
    let Alert = "alert"

type MessageName =
    | Version
    | Verack
    | Addr
    | Inv
    | GetData
    | NotFound
    | GetBlocks
    | GetHeaders
    | Tx
    | Block
    | Header
    | GetAddr
    | MemPool
    | CheckOrder
    | SumbitOrder
    | Reply
    | Ping
    | Pong
    | FilterLoad
    | FilterAdd
    | FilterClear
    | MerkleBlock
    | Alert
    | Unknown of string
    static member Parse messageName =
        match messageName with
        | MessageNames.Version -> Version
        | MessageNames.Verack -> Verack
        | MessageNames.Addr -> Addr
        | MessageNames.Inv -> Inv
        | MessageNames.GetData -> GetData
        | MessageNames.NotFound -> NotFound
        | MessageNames.GetBlocks -> GetBlocks
        | MessageNames.GetHeaders -> GetHeaders
        | MessageNames.Tx -> Tx
        | MessageNames.Block -> Block
        | MessageNames.Header -> Header
        | MessageNames.GetAddr -> GetAddr
        | MessageNames.MemPool -> MemPool
        | MessageNames.CheckOrder -> CheckOrder
        | MessageNames.SumbitOrder -> SumbitOrder
        | MessageNames.Reply -> Reply
        | MessageNames.Ping -> Ping
        | MessageNames.Pong -> Pong
        | MessageNames.FilterLoad -> FilterLoad
        | MessageNames.FilterAdd -> FilterAdd
        | MessageNames.FilterClear -> FilterClear
        | MessageNames.MerkleBlock -> MerkleBlock
        | MessageNames.Alert -> Alert
        | _ -> Unknown messageName

type Version106 =
    { AddressFrom: NetworkAddress
      Nonce: uint64
      UserAgent: string
      StartHeight: int
      Relay: bool }
    member x.Serialize() =
        [| yield! x.AddressFrom.Serialize()
           yield! BitConverter.GetBytes(x.Nonce)
           yield! Conversion.stringToVariableLengthString x.UserAgent
           yield! BitConverter.GetBytes(x.StartHeight)
           yield! BitConverter.GetBytes(x.Relay) |]
    interface IBinarySerializable with
        member x.Serialize() = x.Serialize()

    static member CreateVersion106 fromAddress port  =
        { AddressFrom = NetworkAddress.GetNetworkAddress fromAddress port
          Nonce = Crypto.CreateNonce64()
          UserAgent = "CryptoCurrFun"
          StartHeight = 0
          Relay = false }

    static member Parse offSet buffer version =
        let networkAddress, offset = NetworkAddress.Parse offSet buffer version
        let nonce, offSet = Conversion.bytesToUInt64 offSet buffer
        let userAgent, offSet = Conversion.variableLengthStringToString offSet buffer
        let startHeight, offSet = Conversion.bytesToInt32 offSet buffer
        let relay = buffer.[offSet] = 0uy |> not
        { AddressFrom = networkAddress
          Nonce = nonce
          UserAgent = userAgent
          StartHeight = startHeight
          Relay = relay },
        offSet + 1

type Version =
    { Version: int
      Service: uint64
      Timestamp: int64
      AddressReceive: NetworkAddress
      Extras106: option<Version106> }
    member x.Serialize() =
        let extras = 
            match x.Extras106 with
            | Some x -> x.Serialize()
            | None -> [||]
        [| yield! BitConverter.GetBytes(x.Version)
           yield! BitConverter.GetBytes(x.Service)
           yield! BitConverter.GetBytes(x.Timestamp)
           yield! x.AddressReceive.Serialize()
           yield! extras |]
    interface IBinarySerializable with
        member x.Serialize() = x.Serialize()

    static member Parse buffer =
        let version, offSet = Conversion.bytesToInt32 0 buffer
        let service, offSet = Conversion.bytesToUInt64 offSet buffer
        let timestamp, offSet = Conversion.bytesToInt64 offSet buffer
        let networkAddress, offset = NetworkAddress.Parse offSet buffer version
        let extras, offSet = 
            if version >= 106 then
                let extra, offSet = Version106.Parse offSet buffer version
                Some extra, offSet
            else
                None, offSet
        { Version = version
          Service = service
          Timestamp = timestamp
          AddressReceive = networkAddress
          Extras106 = extras }
    
    static member CreateMyVersion receiveAddress receivePort fromAddress fromPort =
        let extras = Version106.CreateVersion106 fromAddress fromPort
        { Version = Global.ProtocolVersion
          Service = 1uL
          Timestamp = Time.getUnixTimeNow()
          AddressReceive = NetworkAddress.GetNetworkAddress receiveAddress receivePort
          Extras106 = Some extras }

type Address = //addr 
    { Count: int64
      AddressList: NetworkAddress[] }
    static member Parse buffer offSet =
        let count, offSet = Conversion.decodeVariableLengthInt offSet buffer
        let addresses, offSet =
            Conversion.parseBuffer buffer offSet (int count) 
              (fun offSet buffer -> NetworkAddress.Parse offSet buffer Global.ProtocolVersion)
        { Count = count
          AddressList = addresses |> Seq.toArray }, offSet

type InventoryDetails = // inv, getdata & notfound (at the moment don't see how getdata is useful)
    { Count: int64
      Invertory: InventoryVector[] }
    static member Parse buffer offSet =
        let count, offSet = Conversion.decodeVariableLengthInt offSet buffer
        let invs, offSet =
            Conversion.parseBuffer buffer offSet (int count) InventoryVector.Parse
        { Count = count
          Invertory = invs |> Seq.toArray }, offSet


type GetSpec = // getblocks , getheaders
    { Version: uint32
      HashCount: uint64
      BlockLocatorHashes: byte[][]
      HashStop: byte[] }
    static member Create() =
      { Version = uint32 Global.ProtocolVersion
        HashCount = 0uL
        BlockLocatorHashes = [||]
        HashStop = [||] }

type Headers = // headers
    { Count: uint32
      Headers: Header[] }

type PingPong = // ping pong
    { Nonce: uint64 }
    member x.Serialize() =
        BitConverter.GetBytes(x.Nonce)
    interface IBinarySerializable with
        member x.Serialize() = x.Serialize()
    static member Create()  =
        { Nonce = Crypto.CreateNonce64() }
    static member Parse buffer =
        let nonce, offSet = Conversion.bytesToUInt64 0 buffer
        { Nonce = nonce }

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
