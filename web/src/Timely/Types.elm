module Timely.Types exposing (Id(..), Token, Valid(..), decodeId, decodeValid, encodeId, encodeValid, idValue)

import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Encode as Encode


type Id a
    = Id String


type alias Token a =
    Id a


idValue : Id a -> String
idValue (Id a) =
    a


encodeId : Id a -> Encode.Value
encodeId (Id s) =
    Encode.string s


decodeId : Decoder (Id a)
decodeId =
    Decode.map Id Decode.string


type Valid a
    = Valid String


decodeValid : Decoder (Valid a)
decodeValid =
    Decode.map Valid string


encodeValid : Valid a -> Encode.Value
encodeValid (Valid t) =
    Encode.string t
