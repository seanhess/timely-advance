module Timely.Types.Date exposing (Date, decodeDate, formatDate)

import Iso8601
import Task
import Time exposing (Month(..))



-- Dates in UTC
-- always use UTC with these


type alias Date =
    Time.Posix



-- type alias TimeZone =
--     Time.Zone


decodeDate =
    Iso8601.decoder



-- Dates ----------------------------


formatDate : Date -> String
formatDate time =
    let
        formatYear t =
            String.fromInt <| Time.toYear Time.utc time

        formatMonth t =
            case Time.toMonth Time.utc t of
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
            String.fromInt <| Time.toDay Time.utc time
    in
    formatMonth time ++ " " ++ formatDay time ++ ", " ++ formatYear time ++ " "



-- timezone : (TimeZone -> msg) -> Cmd msg
-- timezone toMsg =
--     Task.perform toMsg Time.here
