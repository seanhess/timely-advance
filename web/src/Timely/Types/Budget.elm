module Timely.Types.Budget exposing (Budget, BudgetId, BudgetInfo, BudgetType(..), Scheduled, decodeBudget, decodeBudgetInfo, decodeScheduled, encodeBudget, info, scheduledDate)

import Json.Decode as Decode exposing (Decoder, bool, field, int, list, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Timely.Types exposing (Id, decodeId, encodeId)
import Timely.Types.Date exposing (Date, decodeDate)
import Timely.Types.Money exposing (Money, decodeMoney, encodeMoney)
import Timely.Types.Transactions exposing (Schedule, decodeSchedule, encodeSchedule)


type BudgetId
    = BudgetId


type alias BudgetInfo =
    { name : String
    , schedule : Schedule
    , amount : Money
    }


type alias Budget =
    { budgetId : Id BudgetId
    , name : String
    , schedule : Schedule
    , amount : Money
    }


info : Budget -> BudgetInfo
info b =
    { name = b.name
    , schedule = b.schedule
    , amount = b.amount
    }


type Scheduled a
    = Scheduled Date a


scheduledDate : Scheduled a -> Date
scheduledDate (Scheduled d _) =
    d


type BudgetType
    = Income
    | Expense


decodeBudget : Decoder Budget
decodeBudget =
    Decode.succeed Budget
        |> required "budgetId" decodeId
        |> required "name" string
        |> required "schedule" decodeSchedule
        |> required "amount" decodeMoney


decodeBudgetInfo : Decoder BudgetInfo
decodeBudgetInfo =
    Decode.succeed BudgetInfo
        |> required "name" string
        |> required "schedule" decodeSchedule
        |> required "amount" decodeMoney


decodeScheduled : Decoder a -> Decoder (Scheduled a)
decodeScheduled decodeItem =
    Decode.map2 Scheduled
        (field "date" decodeDate)
        decodeItem


encodeBudget : BudgetInfo -> Encode.Value
encodeBudget b =
    Encode.object
        [ ( "name", Encode.string b.name )
        , ( "schedule", encodeSchedule b.schedule )
        , ( "amount", encodeMoney b.amount )
        ]
