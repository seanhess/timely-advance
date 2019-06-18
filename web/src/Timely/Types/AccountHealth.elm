module Timely.Types.AccountHealth exposing (AccountHealth, decodeAccountHealth)

import Json.Decode as Decode exposing (Decoder, at, bool, fail, field, int, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (Value)
import Time exposing (Weekday(..))
import Timely.Types.Advance exposing (Advance, decodeAdvance)
import Timely.Types.Budget exposing (Budget, BudgetType(..), Scheduled, decodeBudget, decodeScheduled)
import Timely.Types.Daily exposing (Daily, DailyBalance, decodeDaily, decodeDailyBalance)
import Timely.Types.Date exposing (Date, decodeDate)
import Timely.Types.Money exposing (Money, decodeMoney, encodeMoney, toCents)
import Timely.Types.Transactions exposing (Schedule, Transaction, decodeSchedule, decodeTransaction, encodeSchedule)


type alias AccountHealth =
    { balance : Money
    , minimum : Money
    , spendingDaily : Money
    , spendingTotal : Money
    , dailyBalances : List DailyBalance
    , advance : Maybe Advance
    , paycheck : Scheduled
    , bills : List Scheduled
    , billsTotal : Money
    , afterPaycheck : Money
    }


decodeAccountHealth : Decoder AccountHealth
decodeAccountHealth =
    Decode.succeed AccountHealth
        |> required "balance" decodeMoney
        |> required "minimum" decodeMoney
        |> required "spendingDaily" decodeMoney
        |> required "spendingTotal" decodeMoney
        |> required "dailyBalances" (list decodeDailyBalance)
        |> required "advance" (nullable decodeAdvance)
        |> required "paycheck" decodeScheduled
        |> required "bills" (list decodeScheduled)
        |> required "billsTotal" decodeMoney
        |> required "afterPaycheck" decodeMoney
