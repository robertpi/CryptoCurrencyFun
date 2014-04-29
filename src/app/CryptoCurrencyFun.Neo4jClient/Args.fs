namespace CryptoCurrencyFun.Neo4jEtl
open UnionArgParser

type Arguments =
    | Database_url of string
    | Bitcoin_file of string
    | Bitcoin_dir of string
    | Message_between of int * int
    | Limit_to of int
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Database_url _ -> "url of target database"
            | Bitcoin_file _ -> "source bitcoin log file"
            | Bitcoin_dir _ -> "directory containing bitcoin logs"
            | Message_between _ -> "just parse message between min and max"
            | Limit_to _ -> "limits the number of message processed"


