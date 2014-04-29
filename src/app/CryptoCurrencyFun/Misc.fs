namespace CryptoCurrencyFun
open System
open System.Collections.Generic
open System.IO
open System.Text

module File =
    let getByteStream file =
        seq { use stream = File.OpenRead(file)
              let read = ref 1
              let buffer: array<byte> = Array.zeroCreate 4096
              while !read > 0 do
                  read := stream.Read(buffer, 0, buffer.Length)
                  yield! buffer.[0 .. !read - 1] }

module Directory =
    let getByteStreamOfFiles dir spec =
        seq { let files = Directory.GetFiles(dir, spec)
              for file in files do
                  yield! File.getByteStream file }


type EnumeratorObserver<'a> (e: IEnumerator<'a>) = 
    let mutable moreAvailable = true
    interface System.Collections.IEnumerator with 
        member x.Current = e.Current :> obj
        member x.MoveNext() = 
            moreAvailable <- e.MoveNext()
            moreAvailable
        member x.Reset() = e.Reset()
    interface IEnumerator<'a> with 
        member x.Current = e.Current
        member x.Dispose() = e.Dispose()
    member x.MoreAvailable = moreAvailable
    static member Create e = new EnumeratorObserver<'a>(e) 

module Enumerator =
    let skip count (e: IEnumerator<'a>) =
        for x in 0 .. count - 1 do
                  if not (e.MoveNext()) then
                      failwith "no more data available in stream"

    let take count (e: IEnumerator<'a>) =
        let buffer: array<'a> = Array.zeroCreate count

        let mutable readBytes = 0 
        for x in 0 .. count - 1 do
            if e.MoveNext() then
                readBytes  <- readBytes + 1
                buffer.[x] <- e.Current

        if readBytes = count then buffer
        elif readBytes = 0 then Array.zeroCreate 0
        else buffer.[0 .. readBytes - 1]

module Conversion =
    let bytesToHexString (data: array<byte>) =
        let builder = new StringBuilder()
        for b in data do
            builder.Append(sprintf "%02x" b) |> ignore
        builder.ToString()

    let littleEndianBytesToHexString (data: array<byte>) =
        bytesToHexString (data |> Array.rev)

    let hexStringToBytes (data: String) =
        [| for i in 0 .. 2 .. data.Length - 1 do
            yield Convert.ToByte(data.[i .. i + 1], 16) |]

    let readByteBlock offSet length (bytesToProcess: array<byte>) =
        bytesToProcess.[offSet .. offSet + length - 1], offSet + length

    let private bitShifts = Array.init 8 (fun x -> x * 8)
    let inline private genericConvert toInt startIndex length (parts: array<byte>) =
        seq { for i in 0 .. length - 1 do
                yield toInt parts.[startIndex + i] <<< bitShifts.[i] }
        |> Seq.sum, startIndex + length

    let bytesToInt16 startIndex (parts: array<byte>) =
        genericConvert int16 startIndex 2 parts

    let bytesToUInt16 startIndex (parts: array<byte>) =
        genericConvert uint16 startIndex 2 parts

    let bytesToInt32 startIndex (parts: array<byte>) =
        genericConvert int startIndex 4 parts

    let bytesToUInt32 startIndex (parts: array<byte>) =
        genericConvert uint32 startIndex 4 parts

    let bytesToInt64 startIndex (parts: array<byte>) =
        genericConvert int64 startIndex 8 parts

    let bytesToUInt64 startIndex (parts: array<byte>) =
        genericConvert uint64 startIndex 8 parts

    let unixEpoc = new DateTime(1970, 1, 1)
    let dateTimeOfUnixEpoc (i: int) = unixEpoc.AddSeconds(float i)
    let unixEpocOfDateTime (d: DateTime) = 
        let span = d - unixEpoc
        span.TotalSeconds |> int

    let decodeVariableLengthInt startIndex (bytes: byte[]) =
        match bytes.[startIndex] with
        | x when x < 0xfduy -> uint64 x, startIndex + 1
        | x when x = 0xfduy -> 
            // note: could have directly converted using bytesToUInt16, but wasn't sure if there were endian problems here
            let intBlock, offSet = readByteBlock (startIndex + 1) 2 bytes
            let res, _ = bytesToUInt16 0 intBlock 
            uint64 res, offSet
        | x when x = 0xfeuy -> 
            let intBlock, offSet = readByteBlock (startIndex + 1) 4 bytes
            let res, _ = bytesToUInt32 0 intBlock 
            uint64 res, offSet
        | x when x = 0xffuy ->
            let intBlock, offSet = readByteBlock (startIndex + 1) 8 bytes
            let res, _ = bytesToUInt64 0 intBlock 
            res, offSet
        | _ -> failwith "unexpectedly large byte :)"

    let encodeVariableLengthInt (i: uint64) =
        match i with
        | _ when i < 0xfduL -> [| byte i |]
        | _ when i <= 0xffffuL -> [| yield 0xfduy; yield! BitConverter.GetBytes(i) |> Seq.take 2 |]
        | _ when i <= 0xffffffffuL -> [| yield 0xfeuy; yield! BitConverter.GetBytes(i) |> Seq.take 4 |]
        | _ -> [| yield 0xffuy; yield! BitConverter.GetBytes(i) |]

    let stringToVariableLengthString (s: String) =
        let bytes = Encoding.ASCII.GetBytes s
        let i = uint64 bytes.LongLength
        [| yield! encodeVariableLengthInt i; yield! bytes |]

    let variableLengthStringToString offSet buffer =
        let longLength, offSet = decodeVariableLengthInt offSet buffer
        let length = int longLength
        let stringBytes = buffer.[offSet .. offSet + length]
        Encoding.ASCII.GetString(stringBytes), offSet + length + 1

    let parseBuffer buffer offSet count parserFunc =
        let rec inputsLoop remaining offSet acc =
            if remaining > 0 then
                let netAddr, offSet = parserFunc offSet buffer 

                let remaining' = remaining - 1
                let acc' = netAddr :: acc
                inputsLoop remaining' offSet acc'
            else
                acc |> List.rev, offSet
        inputsLoop count offSet []


module Time =
    let private epoch = new DateTime(1970, 1, 1, 0, 0, 0, 0)

    let getUnixTime (date: DateTime) =
        let span = date - epoch
        span.TotalSeconds |> int64

    let getUnixTimeNow () =
        getUnixTime DateTime.Now

[<Interface>]
type IVector =
    abstract member Bytes: array<byte> 
    abstract member AsInt32: int

type Vector(b1) =
    member x.Bytes = [|b1|] 
    member x.AsInt32 =
        if b1 >= 0x80uy then
            (int (b1 &&& 0x80uy)) * -1
        else
            int b1
    override x.ToString() = "v" + (string x.AsInt32)
    interface IVector with
        member x.Bytes = x.Bytes
        member x.AsInt32 = x.AsInt32

type Vector2(b1, b2) =
    member x.Bytes = [|b1;b2;|] 
    member x.AsInt32 =
        let rawUInt16, _ = Conversion.bytesToUInt16 0 x.Bytes
        if rawUInt16 >= 0x8000us then
            (int (rawUInt16 &&& 0x8000us)) * -1
        else
            int rawUInt16
    override x.ToString() = "v" + (string x.AsInt32)
    interface IVector with
        member x.Bytes = x.Bytes
        member x.AsInt32 = x.AsInt32


type Vector4(b1, b2, b3, b4) =
    member x.Bytes = [|b1;b2;b3;b4;|] 
    member x.AsInt32 =
        let rawUInt32, _ = Conversion.bytesToUInt32 0 x.Bytes
        if rawUInt32 >= 0x80000000u then
            (int (rawUInt32 &&& 0x80000000u)) * -1
        else
            int rawUInt32
    override x.ToString() = "v" + (string x.AsInt32)
    interface IVector with
        member x.Bytes = x.Bytes
        member x.AsInt32 = x.AsInt32
