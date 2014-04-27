namespace BitcoinFs.Messages
open System
open System.Security.Cryptography
open NLog
open BitcoinFs
open BitcoinFs.Constants

module Messages =
    let logger = LogManager.GetLogger("Messages")

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
    { Count: uint64
      AddressList: NetworkAddress[] }
    static member Parse buffer offSet =
        let count, offSet = Conversion.decodeVariableLengthInt offSet buffer
        let addresses, offSet =
            Conversion.parseBuffer buffer offSet (int count) 
              (fun offSet buffer -> NetworkAddress.Parse offSet buffer Global.ProtocolVersion)
        { Count = count
          AddressList = addresses |> Seq.toArray }, offSet
    member x.Serialize() =
        [| yield! Conversion.encodeVariableLengthInt(x.Count)
           yield! x.AddressList |> Seq.map (fun x -> x.Serialize()) |> Seq.concat |]
    interface IBinarySerializable with
        member x.Serialize() = x.Serialize()

type InventoryDetails = // inv, getdata & notfound (at the moment don't see how getdata is useful)
    { Count: uint64
      Invertory: InventoryVector[] }
    static member Create count inventory =
      { Count = count
        Invertory = inventory }
    static member Parse buffer offSet =
        let count, offSet = Conversion.decodeVariableLengthInt offSet buffer
        let invs, offSet =
            Conversion.parseBuffer buffer offSet (int count) InventoryVector.Parse
        { Count = count
          Invertory = invs |> Seq.toArray }, offSet
    member x.Serialize() =
        [| yield! Conversion.encodeVariableLengthInt(x.Count)
           yield! x.Invertory |> Seq.map (fun x -> x.Serialize()) |> Seq.concat |]
    interface IBinarySerializable with
        member x.Serialize() = x.Serialize()


type GetSpec = // getblocks , getheaders
    { Version: uint32
      HashCount: uint64
      BlockLocatorHashes: byte[][]
      HashStop: byte[] }
    static member Create version count blockLocatorHashes hashStop =
      { Version = version
        HashCount = count
        BlockLocatorHashes = blockLocatorHashes
        HashStop = hashStop }
    member x.Serialize() =
        [| yield! BitConverter.GetBytes(x.Version)
           yield! Conversion.encodeVariableLengthInt(x.HashCount)
           yield! Seq.concat x.BlockLocatorHashes
           yield! x.HashStop |]
    interface IBinarySerializable with
        member x.Serialize() = x.Serialize()

type Headers = // headers
    { Count: uint64
      Headers: Header[] }
    static member Parse buffer offSet =
        let count, offSet = Conversion.decodeVariableLengthInt offSet buffer
        let headers, offSet =
            Conversion.parseBuffer buffer offSet (int count) Header.Parse
        { Count = count
          Headers = headers |> Seq.toArray }, offSet
    member x.Serialize() =
        [| yield! Conversion.encodeVariableLengthInt(x.Count)
           yield! x.Headers |> Seq.map (fun x -> x.Serialize()) |> Seq.concat |]
    interface IBinarySerializable with
        member x.Serialize() = x.Serialize()

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
      OutputScriptLength: uint64
      OutputScript: array<byte>
      ParsedOutputScript: option<array<Op>>
      CanonicalOutputScript: option<CanonicalOutputScript> }
    static member Parse offSet buffer =
        let output, offSet = Conversion.bytesToInt64 offSet buffer
        
        Messages.logger.Debug(sprintf "output 0x%x" offSet)
        let challengeScriptLength, offSet = Conversion.decodeVariableLengthInt offSet buffer
        Messages.logger.Debug(sprintf "challengeScriptLength value 0x%x"  challengeScriptLength)

        let challengeScriptLengthInt = int challengeScriptLength
        
        Messages.logger.Debug(sprintf "challengeScript 0x%x" offSet)
        let challengeScript, offSet = Conversion.readByteBlock offSet challengeScriptLengthInt buffer

        let parsedScript = ScriptParser.parseScript challengeScript
        let canonicalOutputScript = Option.bind ScriptParser.parseStandardOutputScript parsedScript

        { Value = output
          OutputScriptLength = challengeScriptLength
          OutputScript =  challengeScript
          ParsedOutputScript = parsedScript
          CanonicalOutputScript =  canonicalOutputScript }, 
        offSet

type Input =
    { InputHash: array<byte>
      InputTransactionIndex: int
      ResponseScriptLength: uint64
      ResponseScript: array<byte>
      ParsedResponseScript: option<array<Op>>
      SequenceNumber: int }
    static member Parse offSet buffer =
        let inputHash, offSet = Conversion.readByteBlock offSet 32 buffer

        Messages.logger.Debug(sprintf "inputTransactionIndex 0x%x" offSet)
        let inputTransactionIndex, offSet = Conversion.bytesToInt32 offSet buffer

        Messages.logger.Debug(sprintf "inputTransactionIndex value 0x%x" inputTransactionIndex)

        Messages.logger.Debug(sprintf "responseScriptLength 0x%x" offSet)
        let responseScriptLength, offSet = Conversion.decodeVariableLengthInt offSet buffer
        let responseScriptLengthInt = int responseScriptLength // assume responseScriptLength will always fit into an int32
        
        Messages.logger.Debug(sprintf "responseScript 0x%x responseScriptLengthInt %x" offSet responseScriptLengthInt)
        let responseScript, offSet = Conversion.readByteBlock offSet responseScriptLengthInt  buffer
        
        Messages.logger.Debug(sprintf "sequenceNumber 0x%x" offSet)
        let sequenceNumber, offSet = Conversion.bytesToInt32 offSet buffer

        // nice sanity check that we've correctly found end of the responseScript
        //if sequenceNumber <> -1 then failwith "sequenceNumber number is unused so should always be -1"

        let parsedScript = ScriptParser.parseScript responseScript

        { InputHash = inputHash
          InputTransactionIndex = inputTransactionIndex
          ResponseScriptLength = responseScriptLength
          ResponseScript = responseScript
          ParsedResponseScript = parsedScript
          SequenceNumber = sequenceNumber },
        offSet

type Transaction = 
    { TransactionVersion: int
      NumberOfInputs: uint64
      Inputs: array<Input>
      NumberOfOutputs: uint64
      Outputs: array<Output>
      LockTime: int
      TransactionHash: array<byte> }
    static member Parse offSet buffer =
        let initalOffSet = offSet

        Messages.logger.Debug(sprintf "transactionVersion 0x%x" offSet)
        let transactionVersion, offSet = Conversion.bytesToInt32 offSet buffer

        Messages.logger.Debug(sprintf "numberOfInputs 0x%x" offSet)
        let numberOfInputs, offSet = Conversion.decodeVariableLengthInt offSet buffer
        let inputs, offSet =
            Conversion.parseBuffer buffer offSet (int numberOfInputs) Input.Parse

        Messages.logger.Debug(sprintf "numberOfOutputs 0x%x" offSet)
        let numberOfOutputs, offSet = Conversion.decodeVariableLengthInt offSet buffer
        let outputs, offSet =
            Conversion.parseBuffer buffer offSet (int numberOfOutputs) Output.Parse

        Messages.logger.Debug(sprintf "lockTime 0x%x" offSet)
        let lockTime, offSet = Conversion.bytesToInt32 offSet buffer
        
        let sha256 = SHA256.Create()
        let transactionHash = sha256.ComputeHash(sha256.ComputeHash(buffer, initalOffSet, offSet - initalOffSet))

        Messages.logger.Debug(sprintf "final from transaction 0x%x lockTime 0x%x" offSet lockTime)
        
        { TransactionVersion = transactionVersion
          NumberOfInputs = numberOfInputs
          Inputs = inputs |> Array.ofList
          NumberOfOutputs = numberOfOutputs
          Outputs = Array.ofList outputs
          LockTime = lockTime
          TransactionHash = transactionHash }, 
        offSet

type Block =
    { OffSet: int64
      Length: int
      Version: int
      Hash: array<byte>
      MerKleRoot: array<byte>
      Timestamp: DateTime
      Target: int
      Nonce: int
      NumberOfTransactions: uint64
      Transactions: array<Transaction> }
    static member Parse offSet buffer =
        let initOffSet = offSet

        let version, offSet = Conversion.bytesToInt32 0 buffer

        Messages.logger.Debug(sprintf "hash 0x%x" offSet)
        let hash, offSet = Conversion.readByteBlock offSet 32 buffer

        Messages.logger.Debug(sprintf "merKleRoot 0x%x" offSet)
        let merkleRoot, offSet = Conversion.readByteBlock offSet 32 buffer
        
        Messages.logger.Debug(sprintf "timestamp 0x%x" offSet)
        let timestampInt, offSet = Conversion.bytesToInt32 offSet buffer
        let timestamp = Conversion.dateTimeOfUnixEpoc timestampInt
        
        Messages.logger.Debug(sprintf "target 0x%x" offSet)
        let target, offSet = Conversion.bytesToInt32 offSet buffer
        
        Messages.logger.Debug(sprintf "nonce 0x%x" offSet)
        let nonce, offSet = Conversion.bytesToInt32 offSet buffer
        
        Messages.logger.Debug(sprintf "numberOfTransactions 0x%x" offSet)
        let numberOfTransactions, offSet = Conversion.decodeVariableLengthInt offSet buffer

        Messages.logger.Debug(sprintf "numberOfTransactions value 0x%x" numberOfTransactions)

        let transactions, offSet =
            Conversion.parseBuffer buffer offSet (int numberOfTransactions) Transaction.Parse

        { OffSet = initOffSet
          Length = buffer.Length
          Version = version
          Hash = hash
          MerKleRoot = merkleRoot
          Timestamp = timestamp
          Target = target
          Nonce = nonce
          NumberOfTransactions = numberOfTransactions
          Transactions = Array.ofList transactions },
        offSet

