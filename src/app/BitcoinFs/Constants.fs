namespace BitcoinFs.Constants
module MagicNumbers =
    let Bitcoin = 0xD9B4BEF9u
    let BitcoinTestnet = 0xDAB5BFFAu
    let BitcoinTestnet3 = 0x0709110Bu
    let Namecoin  = 0xFEB4BEF9u

module Ports =
    let Bitcoin = 8332
    let BitcoinTestnet = 18332
    let BitcoinTestnet3 = 38332
    let Namecoin  = 8332

module SeedDns =
    let Bitcoin =
        [ "bitseed.xf2.org";
          "dnsseed.bluematt.me";
          "seed.bitcoin.sipa.be";
          "dnsseed.bitcoin.dashjr.org";
          "seed.bitcoinstats.com" ]
