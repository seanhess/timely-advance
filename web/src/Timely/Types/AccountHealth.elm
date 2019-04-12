module Timely.Types.AccountHealth exposing (AccountHealth, Bill, Budget, decodeAccountHealth, decodeBudget, encodeBudget)

import Json.Decode as Decode exposing (Decoder, at, bool, fail, field, int, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (Value)
import Time exposing (Weekday(..))
import Timely.Types.Date exposing (Date, decodeDate)
import Timely.Types.Money exposing (Money, decodeMoney, encodeMoney)
import Timely.Types.Transactions exposing (Schedule, Transaction, decodeSchedule, decodeTransaction, encodeSchedule)


type alias AccountHealth =
    { balance : Money
    , budgeted : Money
    , spending : Money
    , income : Budget
    , bills : List Bill
    , paychecks : List Transaction
    }


type alias Bill =
    { saved : Money
    , next : Date
    , budget : Budget
    }


type alias Budget =
    { name : String
    , schedule : Schedule
    , amount : Money
    }


decodeAccountHealth : Decoder AccountHealth
decodeAccountHealth =
    Decode.succeed AccountHealth
        |> required "balance" decodeMoney
        |> required "budgeted" decodeMoney
        |> required "spending" decodeMoney
        |> required "income" decodeBudget
        |> required "bills" (list decodeBill)
        |> required "paychecks" (list decodeTransaction)


decodeBudget : Decoder Budget
decodeBudget =
    Decode.succeed Budget
        |> required "name" string
        |> required "schedule" decodeSchedule
        |> required "amount" decodeMoney


decodeBill : Decoder Bill
decodeBill =
    Decode.succeed Bill
        |> required "saved" decodeMoney
        |> required "next" decodeDate
        |> required "budget" decodeBudget


encodeBudget : Budget -> Value
encodeBudget b =
    Encode.object
        [ ( "name", Encode.string b.name )
        , ( "schedule", encodeSchedule b.schedule )
        , ( "amount", encodeMoney b.amount )
        ]
