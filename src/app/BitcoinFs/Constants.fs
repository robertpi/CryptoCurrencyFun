namespace BitcoinFs.Constants

module Global =
    let ProtocolVersion = 70002

module MagicNumbers =
    let Bitcoin = 0xD9B4BEF9u
    let BitcoinTestnet = 0xDAB5BFFAu
    let BitcoinTestnet3 = 0x0709110Bu
    let Namecoin  = 0xFEB4BEF9u

    let Litecoin = 0xDBB6C0FBu
    let LitecoinTestnet = 0xDCB7C1FCu

module Ports =
    let Bitcoin = 8333
    let BitcoinTestnet = 18333
    let BitcoinTestnet3 = 38333
    let Namecoin  = 8332

    let Litecoin = 9333

module SeedDns =
    let Bitcoin =
        [ "bitseed.xf2.org";
          "dnsseed.bluematt.me";
          "seed.bitcoin.sipa.be";
          "dnsseed.bitcoin.dashjr.org";
          "seed.bitcoinstats.com" ]

    let Litecoin =
      [ "dnsseed.litecointools.com";
        "dnsseed.litecoinpool.org";
        "dnsseed.ltc.xurious.com";
        "dnsseed.koin-project.com";
        "dnsseed.weminemnc.com" ]