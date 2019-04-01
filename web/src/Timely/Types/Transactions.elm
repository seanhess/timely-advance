module Timely.Types.Transactions exposing (Group, History, Transaction, decodeTransaction)

import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Time
import Timely.Types.Date exposing (Date, decodeDate)
import Timely.Types.Money exposing (Money, decodeMoney)


type alias History =
    { income : List Group
    , expenses : List Group
    }


type alias Group =
    { name : String
    , average : Money
    , total : Money
    , transactions : Transaction
    }


type alias Transaction =
    { transactionId : String
    , date : Date
    , category : String
    , pending : Bool
    , amount : Money
    , name : String
    }


decodeTransaction : Decoder Transaction
decodeTransaction =
    Decode.succeed Transaction
        |> required "transactionId" string
        |> required "date" decodeDate
        |> required "category" string
        |> required "pending" bool
        |> required "amount" decodeMoney
        |> required "name" string
