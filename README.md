CryptoCurrencyFun
=================

CryptoCurrencyFun is a library for working with crypto currencies in .NET. The library is implemented using F#, but was designed to be usable from C# and other .NET languages.

The canonical example of a crypto currency is [Bitcoin](https://bitcoin.org/), although [there](https://litecoin.org/) [are](http://dogecoin.com/) [plenty](http://www.peercoin.net/) [of](http://namecoin.info/) [others](http://www.nxtcommunity.org/).
Most other crypto currencies are forks of Bitcoin and use the same protocol as Bitcoin, so generally this library will attempt to be compitable with as many as possibly. Currently it is being tested with Bitcoin, Litecoin and Dogecoin, and hopefully this will expand over time.

Any crypto currency that is a fork of Bitcoin is based a round a [block chain](https://en.bitcoin.it/wiki/Block_chain). This is a ledger, or database, of all transaction that have taken place using the currency. The core of any Bitcoin-esque system is a peer to peer protocol for read and writing to the block chain. The ledger is broken down into two parts transactions and blocks. Transactions are where coins are passed between users of the currency and blocks group transactions together into a structure that can be written to the ledger because it has rare hash that been found by the currency's [miners](http://en.wikipedia.org/wiki/Bitcoin#Mining). A library for working with crypto currencies will need to provide tools for handling these concepts. 

CryptoCurrencyFun provides, or will provide, the following types for working with crypto currencies:

- A set of types that represent the various messages that pass over the peer to peer network, for the moment these live in [Messages.fs](https://github.com/robertpi/CryptoCurrencyFun/blob/master/src/app/CryptoCurrencyFun/Messages.fs)

- A type for connecting to the peer to peer network. This is PeerToPeerConnectionManager that lives in [PeerToPeer.fs](https://github.com/robertpi/CryptoCurrencyFun/blob/master/src/app/CryptoCurrencyFun/PeerToPeer.fs)

- A type for managing the transactions that are not yet written to the block chain. This is called MemoryPool and is in the process of being implemented, see [MemoryPool.fs](https://github.com/robertpi/CryptoCurrencyFun/blob/master/src/app/CryptoCurrencyFun/MemoryPool.fs)

- A type for writing to the block chain. This does not exist yet.

- A type to read from the block chain. Currenctly you can read sequentially from the block chain using BlockParserStream from [BlockParser.fs](https://github.com/robertpi/CryptoCurrencyFun/blob/master/src/app/CryptoCurrencyFun/BlockParser.fs). While sequential access is sometimes useful, it will be necessary to provide indexed access to the blockchain and this not currently implemented.

- A type to generate and store new key public/private keys pairs and address. So far we can convert a public to address, see [Address.fs](https://github.com/robertpi/CryptoCurrencyFun/blob/master/src/app/CryptoCurrencyFun/Address.fs), but nothing else.

- A type to create and sign new transactions. This does not exist.

- Some types for mining new blocks, this is not implemented, and is low priorty. We'll be concentrating on functionality to read and write from the network and may add mining functionlity later.
