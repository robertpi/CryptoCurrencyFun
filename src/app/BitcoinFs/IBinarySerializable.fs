namespace BitcoinFs 

type IBinarySerializable =
    abstract Serialize: unit -> array<byte>

