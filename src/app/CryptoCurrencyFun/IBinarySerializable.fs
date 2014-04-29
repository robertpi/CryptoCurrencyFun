namespace CryptoCurrencyFun 

type IBinarySerializable =
    abstract Serialize: unit -> array<byte>

