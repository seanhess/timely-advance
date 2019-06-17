module Timely.Types.Advance exposing (Advance, AdvanceId, decodeAdvance)

import Json.Decode as Decode exposing (Decoder, nullable)
import Json.Decode.Pipeline exposing (..)
import Timely.Types exposing (Id(..), decodeId)
import Timely.Types.Date exposing (Date, decodeDate)
import Timely.Types.Money exposing (Money, decodeMoney)


type AdvanceId
    = AdvanceId


type alias Advance =
    { advanceId : Id AdvanceId

    -- , accountId : Id AccountId
    , amount : Money
    , offer : Money
    , due : Date
    , offered : Date
    , activated : Maybe Date
    , collected : Maybe Date
    }


decodeAdvance : Decoder Advance
decodeAdvance =
    Decode.succeed Advance
        |> required "advanceId" decodeId
        -- |> required "accountId" decodeId
        |> required "amount" decodeMoney
        |> required "offer" decodeMoney
        |> required "due" decodeDate
        |> required "offered" decodeDate
        |> required "activated" (nullable decodeDate)
        |> required "collected" (nullable decodeDate)
