namespace BitcoinFs.Messages
open System
open System.Diagnostics
open System.Text
open System.Net
open BitcoinFs

type RawMessageHeader =
    { Magic: uint32      // 4
      Command: string    // 16
      Length: uint32     // 20
      Checksum: uint32 } // 24
    static member HeaderLength = 24
    static member Parse buffer =
        let magic, offSet = Conversion.bytesToUInt32 0 buffer
        let commandBytes, offSet = Conversion.readByteBlock offSet 12 buffer
        let trimmedCommandBytes = 
            commandBytes 
            |> Seq.takeWhile (fun x -> x <> 0uy) 
            |> Seq.toArray
        let command = Encoding.ASCII.GetString(trimmedCommandBytes)
        let length, offSet = Conversion.bytesToUInt32 offSet buffer
        let checksum, offSet = Conversion.bytesToUInt32 offSet buffer
        { Magic = magic
          Command = command
          Length = length
          Checksum = checksum }
    member x.Serialize() =
        let commandBytes = Encoding.ASCII.GetBytes(x.Command) 
        let padLength = 12 - commandBytes.Length
        let paddedCommand = 
            if padLength = 0 then
                commandBytes
            else
                [| yield! commandBytes; yield! Array.zeroCreate padLength; |]
        [| yield! BitConverter.GetBytes(x.Magic)
           yield! paddedCommand
           yield! BitConverter.GetBytes(x.Length)
           yield! BitConverter.GetBytes(x.Checksum) |]
    interface IBinarySerializable<RawMessageHeader> with
        member x.Serialize() = x.Serialize()
    static member Create magic command length checkSum =
        { Magic = magic
          Command = command
          Length = length
          Checksum = checkSum }
        
        

type NetworkAddress = 
    { Timestamp: option<uint32>
      Service: uint64
      Address: byte[]
      Port: uint16 }
    member x.Serialize() =
        let timestampBits = 
            match x.Timestamp with 
            | Some x -> BitConverter.GetBytes(x) 
            | None -> [||]
        [| yield! timestampBits
           yield! BitConverter.GetBytes(x.Service)
           yield! x.Address
           yield! BitConverter.GetBytes(x.Port) |]
    interface IBinarySerializable<NetworkAddress> with
        member x.Serialize() = x.Serialize()
    static member GetNetworkAddress (address: IPAddress) port =
        if address = null then failwith "address should not be null"
        let addressBytes = address.GetAddressBytes()
        if addressBytes = null then failwith "addressBytes should not be null"
        let addressPadding = 16 - addressBytes.Length
        let addressBytes =
            if addressPadding > 0 then
                let padding = Array.zeroCreate addressPadding
                [| yield! padding; yield! addressBytes |]
            else
                addressBytes 
        { Timestamp = Some(Time.getUnixTimeNow() |> uint32)
          Service = 1uL
          Address = addressBytes
          Port = port }
    static member Parse offSet buffer version =
        let timestamp, offSet =
            if version >= 31402 then
                let timestamp, offset = Conversion.bytesToUInt32 offSet buffer
                Some timestamp, offset
            else
                None, offSet
        let service, offSet = Conversion.bytesToUInt64 offSet buffer
        let address, offSet = Conversion.readByteBlock offSet 16 buffer
        let port, offSet = Conversion.bytesToUInt16 offSet buffer
        { Timestamp = timestamp
          Service = service
          Address = address
          Port = port},
        offSet

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
