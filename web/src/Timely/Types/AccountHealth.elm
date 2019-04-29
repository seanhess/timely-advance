module Timely.Types.AccountHealth exposing (AccountHealth, Event, Projection, decodeAccountHealth, decodeEvent, decodeProjection, eventType)

import Json.Decode as Decode exposing (Decoder, at, bool, fail, field, int, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (Value)
import Time exposing (Weekday(..))
import Timely.Types.Budget exposing (Budget, BudgetType(..), decodeBudget)
import Timely.Types.Date exposing (Date, decodeDate)
import Timely.Types.Money exposing (Money, decodeMoney, encodeMoney, toCents)
import Timely.Types.Transactions exposing (Schedule, Transaction, decodeSchedule, decodeTransaction, encodeSchedule)


type alias AccountHealth =
    { projection : Projection
    }


type alias Projection =
    { balance : Money
    , lowest : Money
    , events : List Event
    }


type alias Event =
    { transaction : Transaction
    , balance : Money
    , budget : Budget
    }


decodeAccountHealth : Decoder AccountHealth
decodeAccountHealth =
    Decode.succeed AccountHealth
        |> required "projection" decodeProjection


decodeProjection : Decoder Projection
decodeProjection =
    Decode.succeed Projection
        |> required "balance" decodeMoney
        |> required "lowest" decodeMoney
        |> required "events" (list decodeEvent)


decodeEvent : Decoder Event
decodeEvent =
    Decode.succeed Event
        |> required "transaction" decodeTransaction
        |> required "balance" decodeMoney
        |> required "budget" decodeBudget


eventType : Event -> BudgetType
eventType event =
    if toCents event.transaction.amount > 0 then
        Income

    else
        Expense
