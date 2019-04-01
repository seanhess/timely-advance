module Timely.Types.Date exposing (Date, TimeZone, decodeDate, formatDate, timezone)

import Iso8601
import Task
import Time exposing (Month(..))


type alias Date =
    Time.Posix


type alias TimeZone =
    Time.Zone


decodeDate =
    Iso8601.decoder



-- Dates ----------------------------


formatDate : TimeZone -> Date -> String
formatDate zone time =
    let
        formatYear t =
            String.fromInt <| Time.toYear zone time

        formatMonth t =
            case Time.toMonth zone t of
                Jan ->
                    "Jan"

                Feb ->
                    "Feb"

                Mar ->
                    "Mar"

                Apr ->
                    "Apr"

                May ->
                    "May"

                Jun ->
                    "Jun"

                Jul ->
                    "Jul"

                Aug ->
                    "Aug"

                Sep ->
                    "Sep"

                Oct ->
                    "Oct"

                Nov ->
                    "Nov"

                Dec ->
                    "Dec"

        formatDay t =
            String.fromInt <| Time.toDay zone time
    in
    formatMonth time ++ " " ++ formatDay time ++ ", " ++ formatYear time ++ " "


timezone : (TimeZone -> msg) -> Cmd msg
timezone toMsg =
    Task.perform toMsg Time.here
