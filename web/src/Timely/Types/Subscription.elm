module Timely.Types.Subscription exposing (Level(..), Subscription, decode, encodeLevel, formatLevel)

import Json.Decode as Decode exposing (Decoder, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (Value)
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
    | Premium


toLevel : String -> Level
toLevel s =
    case s of
        "Basic" ->
            Basic

        "Premium" ->
            Premium

        _ ->
            Basic


formatLevel : Level -> String
formatLevel l =
    case l of
        Basic ->
            "Basic"

        Premium ->
            "Premium"


decode : Decoder Subscription
decode =
    Decode.succeed Subscription
        |> required "level" decodeLevel
        |> required "cost" decodeMoney
        |> required "limit" decodeMoney


encodeLevel : Level -> Value
encodeLevel level =
    Encode.object
        [ ( "level", Encode.string (formatLevel level) )
        ]


decodeLevel : Decoder Level
decodeLevel =
    Decode.map toLevel string



-- Decode.andThen
