module Timely.Types.Money exposing (Money(..), decodeMoney, encodeMoney, formatCents, formatDollars, formatMoney, formatMoneyNoSign, fromCents, fromDollars, toCents, toDollars, total)

import Json.Decode as Decode exposing (Decoder, int)
import Json.Encode as Encode
import Time


type Money
    = Money Int


total : List Money -> Money
total ms =
    List.map toCents ms |> List.sum |> fromCents


toCents : Money -> Int
toCents (Money c) =
    c


fromCents : Int -> Money
fromCents c =
    Money c


toDollars : Money -> Float
toDollars (Money c) =
    toFloat c / 100


fromDollars : Float -> Money
fromDollars i =
    Money <| round (i * 100)


decodeMoney : Decoder Money
decodeMoney =
    Decode.map Money int


encodeMoney : Money -> Encode.Value
encodeMoney (Money c) =
    Encode.int c


formatMoney : Money -> String
formatMoney m =
    formatSign m ++ formatDollars m ++ "." ++ formatCents m


formatMoneyNoSign : Money -> String
formatMoneyNoSign m =
    formatDollars m ++ "." ++ formatCents m


formatDollars : Money -> String
formatDollars (Money c) =
    String.fromInt (abs c // 100)


formatSign : Money -> String
formatSign (Money c) =
    if c >= 0 then
        "$"

    else
        "-$"


formatCents : Money -> String
formatCents (Money c) =
    String.padLeft 2 '0' <| String.fromInt (modBy 100 (abs c))


negate : Money -> Money
negate (Money c) =
    Money -c
