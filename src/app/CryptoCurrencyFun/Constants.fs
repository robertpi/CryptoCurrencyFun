namespace CryptoCurrencyFun.Constants
open System.Diagnostics
open System.Net
open System.Text.RegularExpressions

module Connection =
  let resolvedAddressRegex = new Regex("Address: (.+)")

  let shellNslookup (addressRegex: Regex) dns =
      let nsProd = Process.Start(new ProcessStartInfo ("nslookup", dns, UseShellExecute = false, RedirectStandardOutput = true))
      let text = nsProd.StandardOutput.ReadToEnd()
      let matches: seq<Match> = addressRegex.Matches text |> Seq.cast
      matches
      |> Seq.map (fun m -> IPAddress.Parse m.Groups.[1].Value)

type NetworkDescription =
    { MagicNumber: uint32
      Port: int
      DnsSeeds: seq<string>
      IpSeeds: seq<IPAddress>
      DnsResolution: string -> seq<IPAddress> }
    static member Create(magicNumber, port, dnsSeed, ?ipSeeds, ?dnsResolution) =  
        let ipSeeds =
            match ipSeeds with
            | Some x -> x
            | None -> Seq.empty
        let dnsResolution =
            match dnsResolution with
            | Some x -> x
            | None -> 
                (fun x ->  
                    let res = Dns.GetHostAddresses x 
                    res :> seq<IPAddress>)
        { MagicNumber = magicNumber
          Port = port
          DnsSeeds = dnsSeed
          IpSeeds = ipSeeds 
          DnsResolution = dnsResolution }
    member x.GetIPAddresses() =
        x.DnsSeeds
        |> Seq.collect x.DnsResolution
        |> (fun resolvedIps -> Seq.append resolvedIps x.IpSeeds)


module MagicNumbers =
    let Bitcoin = 0xD9B4BEF9u
    let BitcoinTestnet = 0xDAB5BFFAu
    let BitcoinTestnet3 = 0x0709110Bu
    let Namecoin  = 0xFEB4BEF9u

    let Litecoin = 0xDBB6C0FBu
    let LitecoinTestnet = 0xDCB7C1FCu

    let Dogecoin = 0xC0C0C0C0u
    let DogecoinTestnet = 0xDCB7C1FCu

module NetworkDetails =
    module internal SeedDns =
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

        let Dogecoin =
          [ "seed.dogecoin.com"; 
            "seed.dogechain.info";
            "seed.mophides.com";
            "seed.dglibrary.org" ]


    let Bitcoin = NetworkDescription.Create(MagicNumbers.Bitcoin, 8333, SeedDns.Bitcoin)
    let BitcoinTestnet = NetworkDescription.Create(MagicNumbers.BitcoinTestnet, 18333, SeedDns.Bitcoin)
    let BitcoinTestnet3 = NetworkDescription.Create(MagicNumbers.BitcoinTestnet3, 38333, SeedDns.Bitcoin)
    let Litecoin = NetworkDescription.Create(MagicNumbers.Litecoin, 9333, SeedDns.Litecoin)
    let LitecoinTestnet = NetworkDescription.Create(MagicNumbers.LitecoinTestnet, 19333, SeedDns.Litecoin)
    let Dogecoin = NetworkDescription.Create(MagicNumbers.Dogecoin, 22556, SeedDns.Dogecoin)

module Global =
    let ProtocolVersion = 70002
