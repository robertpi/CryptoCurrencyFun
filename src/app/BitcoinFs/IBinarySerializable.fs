namespace BitcoinFs 

type IBinarySerializable<'a> =
    abstract Serialize: unit -> array<byte>

