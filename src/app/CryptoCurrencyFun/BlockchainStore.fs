namespace CryptoCurrencyFun
open CryptoCurrencyFun.Messages

type IBlockchainStore =
    abstract IsStoredTransaction: string -> bool
    abstract IsStoredBlock: string -> bool
    abstract WriteBlock: Block