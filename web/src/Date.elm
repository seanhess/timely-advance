module Date exposing (Date, toDateString)

import Html exposing (..)
import Html.Attributes exposing (..)
import Iso8601
import List
import Time exposing (Month(..), utc)
import Validate exposing (Validator)



-- Validate.validate (amountValidator advance account advances) model.acceptAmount


type alias Year =
    Int


type alias Day =
    Int


type alias Month =
    Int


type alias Date =
    String


years : List Year
years =
    List.range 2019 1900


type alias MonthName =
    { num : Int
    , name : String
    }


months : List MonthName
months =
    [ MonthName 1 "January", MonthName 2 "February", MonthName 3 "March", MonthName 4 "April", MonthName 5 "May", MonthName 6 "June", MonthName 7 "July", MonthName 8 "August", MonthName 9 "September", MonthName 10 "October", MonthName 11 "November", MonthName 12 "December" ]


days : List Day
days =
    List.range 1 31


type Invalid
    = Invalid


toDateString : Time.Posix -> String
toDateString t =
    let
        pad x =
            String.padLeft 2 '0' x

        monthNumber month =
            case month of
                Jan ->
                    "01"

                Feb ->
                    "02"

                Mar ->
                    "03"

                Apr ->
                    "04"

                May ->
                    "05"

                Jun ->
                    "06"

                Jul ->
                    "07"

                Aug ->
                    "08"

                Sep ->
                    "09"

                Oct ->
                    "10"

                Nov ->
                    "11"

                Dec ->
                    "12"
    in
    [ String.fromInt (Time.toYear utc t), monthNumber (Time.toMonth utc t), pad (String.fromInt (Time.toDay utc t)) ]
        |> String.join "-"


toTime : Year -> Month -> Day -> Result Invalid Time.Posix
toTime y m d =
    let
        pad x =
            String.padLeft 2 '0' x
    in
    [ String.fromInt y, pad (String.fromInt m), pad (String.fromInt d) ]
        |> String.join "-"
        |> Iso8601.toTime
        |> Result.mapError (\_ -> Invalid)



-- view : Year -> Month -> Day -> Html msg
-- view y m d =
--     div [] []
-- yearSelect : Html msg
-- yearSelect =
--     let
--         opt y =
--             option [ value (String.fromInt y) ] [ text (String.fromInt y) ]
--     in
--     select []
--         (List.map opt years)
-- monthSelect : Html msg
-- monthSelect =
--     let
--         opt m =
--             option [ value (String.fromInt m.num) ] [ text m.name ]
--     in
--     select []
--         (List.map opt months)
