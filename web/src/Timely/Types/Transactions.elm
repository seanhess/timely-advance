module Timely.Types.Transactions exposing (Group, History, Schedule(..), TransRow, Transaction, decodeGroup, decodeHistory, decodeSchedule, decodeTransRow, decodeTransaction, encodeSchedule, formatBiweek, formatDay, formatDaySuffix, formatWeekday, isMonthly, isWeekly)

import Json.Decode as Decode exposing (Decoder, at, bool, fail, field, int, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (Value)
import Time exposing (Weekday(..))
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
    , schedule : Maybe Schedule
    , transactions : List Transaction
    }


type alias TransRow =
    { transactionId : String
    , date : Date
    , category : String
    , pending : Bool
    , amount : Money
    , name : String
    }


type alias Transaction =
    { date : Date
    , amount : Money
    , name : String
    }


type Schedule
    = Weekly WeeklyInfo
    | Biweekly BiweeklyInfo
    | Monthly MonthlyInfo
    | Semimonthly SemimonthlyInfo


type alias WeeklyInfo =
    { weekday : Weekday }


type alias BiweeklyInfo =
    { weekday : Weekday, bi : Biweek }


type alias MonthlyInfo =
    { date : Day }


type alias SemimonthlyInfo =
    { date1 : Day, date2 : Day }


type Biweek
    = A
    | B


type alias Day =
    Int


decodeTransRow : Decoder TransRow
decodeTransRow =
    Decode.succeed TransRow
        |> required "transactionId" string
        |> required "date" decodeDate
        |> required "category" string
        |> required "pending" bool
        |> required "amount" decodeMoney
        |> required "name" string


decodeTransaction : Decoder Transaction
decodeTransaction =
    Decode.succeed Transaction
        |> required "date" decodeDate
        |> required "amount" decodeMoney
        |> required "name" string


decodeGroup : Decoder Group
decodeGroup =
    Decode.succeed Group
        |> required "name" string
        |> required "average" decodeMoney
        |> required "total" decodeMoney
        |> required "schedule" (nullable decodeSchedule)
        |> required "transactions" (list decodeTransaction)


decodeHistory : Decoder History
decodeHistory =
    Decode.succeed History
        |> required "income" (list decodeGroup)
        |> required "expenses" (list decodeGroup)



-- Schedule decoders --------------------------------------


decodeSchedule : Decoder Schedule
decodeSchedule =
    Decode.oneOf
        [ field "weekly" decodeWeekly
        , field "biweekly" decodeBiweekly
        , field "monthly" decodeMonthly
        , field "semimonthly" decodeSemimonthly
        ]


decodeWeekly : Decoder Schedule
decodeWeekly =
    Decode.succeed WeeklyInfo
        |> required "weekday" decodeWeekday
        |> Decode.map Weekly


decodeBiweekly : Decoder Schedule
decodeBiweekly =
    Decode.succeed BiweeklyInfo
        |> required "weekday" decodeWeekday
        |> required "bi" decodeBiweek
        |> Decode.map Biweekly


decodeMonthly : Decoder Schedule
decodeMonthly =
    Decode.succeed MonthlyInfo
        |> required "date" int
        |> Decode.map Monthly


decodeSemimonthly : Decoder Schedule
decodeSemimonthly =
    Decode.succeed SemimonthlyInfo
        |> required "date1" int
        |> required "date2" int
        |> Decode.map Semimonthly


decodeWeekday : Decoder Weekday
decodeWeekday =
    Decode.andThen (fromResult << toWeekday) string


decodeBiweek : Decoder Biweek
decodeBiweek =
    Decode.andThen (fromResult << toBiweek) string


toWeekday : String -> Result String Weekday
toWeekday s =
    case s of
        "Monday" ->
            Ok Mon

        "Tuesday" ->
            Ok Tue

        "Wednesday" ->
            Ok Wed

        "Thursday" ->
            Ok Thu

        "Friday" ->
            Ok Fri

        "Saturday" ->
            Ok Sat

        "Sunday" ->
            Ok Sun

        _ ->
            Err <| "expected Weekday, encountered: " ++ s


toBiweek : String -> Result String Biweek
toBiweek s =
    case s of
        "A" ->
            Ok A

        "B" ->
            Ok B

        _ ->
            Err <| "expected Biweek, encountered: " ++ s


fromResult : Result String a -> Decoder a
fromResult res =
    case res of
        Ok a ->
            Decode.succeed a

        Err e ->
            Decode.fail e


encodeSchedule : Schedule -> Value
encodeSchedule s =
    case s of
        Weekly info ->
            Encode.object [ ( "weekly", encodeWeekly info ) ]

        Biweekly info ->
            Encode.object [ ( "biweekly", encodeBiweekly info ) ]

        Monthly info ->
            Encode.object [ ( "monthly", encodeMonthly info ) ]

        Semimonthly info ->
            Encode.object [ ( "semimonthly", encodeSemimonthly info ) ]


encodeWeekly : WeeklyInfo -> Value
encodeWeekly info =
    Encode.object
        [ ( "weekday", Encode.string <| formatWeekday info.weekday ) ]


encodeBiweekly : BiweeklyInfo -> Value
encodeBiweekly info =
    Encode.object
        [ ( "weekday", Encode.string <| formatWeekday info.weekday )
        , ( "bi", Encode.string <| formatBiweek info.bi )
        ]


encodeMonthly : MonthlyInfo -> Value
encodeMonthly info =
    Encode.object
        [ ( "date", Encode.int info.date ) ]


encodeSemimonthly : SemimonthlyInfo -> Value
encodeSemimonthly info =
    Encode.object
        [ ( "date1", Encode.int info.date1 )
        , ( "date2", Encode.int info.date2 )
        ]


formatBiweek : Biweek -> String
formatBiweek bw =
    case bw of
        A ->
            "A"

        B ->
            "B"


formatWeekday : Weekday -> String
formatWeekday wd =
    case wd of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


formatDay n =
    String.fromInt n ++ formatDaySuffix n


formatDaySuffix n =
    case n of
        1 ->
            "st"

        2 ->
            "nd"

        _ ->
            "th"


isMonthly : Schedule -> Bool
isMonthly s =
    case s of
        Monthly _ ->
            True

        _ ->
            False


isWeekly : Schedule -> Bool
isWeekly s =
    case s of
        Weekly _ ->
            True

        _ ->
            False
