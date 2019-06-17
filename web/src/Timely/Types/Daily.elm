module Timely.Types.Daily exposing (Daily, DailyBalance, decodeDaily, decodeDailyBalance)

import Json.Decode as Decode exposing (Decoder, bool, field, int, list, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Timely.Types.Budget exposing (Budget, decodeBudget)
import Timely.Types.Date exposing (Date, decodeDate)
import Timely.Types.Money exposing (Money, decodeMoney)


type alias Daily =
    { date : Date
    , spending : Money
    , bills : List Budget
    }


type alias DailyBalance =
    { daily : Daily
    , balance : Money
    }


decodeDaily : Decoder Daily
decodeDaily =
    Decode.succeed Daily
        |> required "date" decodeDate
        |> required "spending" decodeMoney
        |> required "bills" (list decodeBudget)


decodeDailyBalance : Decoder DailyBalance
decodeDailyBalance =
    Decode.succeed DailyBalance
        |> required "daily" decodeDaily
        |> required "balance" decodeMoney
