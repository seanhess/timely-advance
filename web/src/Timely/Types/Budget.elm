module Timely.Types.Budget exposing (Budget, BudgetId, BudgetType(..), Scheduled, decodeBudget, decodeBudgetId, decodeScheduledBudget, encodeBudget, toBudget)

import Json.Decode as Decode exposing (Decoder, bool, field, int, list, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Timely.Types exposing (Id, decodeId, encodeId)
import Timely.Types.Date exposing (Date, decodeDate)
import Timely.Types.Money exposing (Money, decodeMoney, encodeMoney)
import Timely.Types.Transactions exposing (Schedule, decodeSchedule, encodeSchedule)


type alias BudgetId a =
    { a | budgetId : Id Budget }


type alias Budget =
    { name : String
    , schedule : Schedule
    , amount : Money
    }


type alias Scheduled a =
    { a | date : Date }


type BudgetType
    = Income
    | Expense


decodeBudget : Decoder Budget
decodeBudget =
    Decode.succeed Budget
        |> required "name" string
        |> required "schedule" decodeSchedule
        |> required "amount" decodeMoney


decodeBudgetId : Decoder (BudgetId Budget)
decodeBudgetId =
    Decode.map2 budgetWithId
        (field "budgetId" decodeId)
        decodeBudget


decodeScheduledBudget : Decoder (Scheduled Budget)
decodeScheduledBudget =
    Decode.map2 scheduledBudget
        (field "date" decodeDate)
        decodeBudget


scheduledBudget : Date -> Budget -> Scheduled Budget
scheduledBudget date { name, schedule, amount } =
    { date = date
    , name = name
    , schedule = schedule
    , amount = amount
    }


budgetWithId : Id Budget -> Budget -> BudgetId Budget
budgetWithId budgetId { name, schedule, amount } =
    { budgetId = budgetId
    , name = name
    , schedule = schedule
    , amount = amount
    }


encodeBudget : Budget -> Encode.Value
encodeBudget b =
    Encode.object
        [ ( "name", Encode.string b.name )
        , ( "schedule", encodeSchedule b.schedule )
        , ( "amount", encodeMoney b.amount )
        ]


toBudget : BudgetId Budget -> Budget
toBudget { name, schedule, amount } =
    { name = name
    , schedule = schedule
    , amount = amount
    }
