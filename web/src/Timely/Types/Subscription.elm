module Timely.Types.Subscription exposing (Level(..), Subscription, decode)

import Json.Decode as Decode exposing (Decoder, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Timely.Types exposing (Id(..), decodeId)
import Timely.Types.Date exposing (Date, decodeDate)
import Timely.Types.Money exposing (Money, decodeMoney)


type alias Subscription =
    { level : Level
    , cost : Money
    , limit : Money
    }


type Level
    = Basic
    | Approved


toLevel : String -> Level
toLevel s =
    case s of
        "Basic" ->
            Basic

        "Approved" ->
            Approved

        _ ->
            Basic


decode : Decoder Subscription
decode =
    Decode.succeed Subscription
        |> required "level" decodeLevel
        |> required "cost" decodeMoney
        |> required "limit" decodeMoney


decodeLevel : Decoder Level
decodeLevel =
    Decode.map toLevel string



-- Decode.andThen
