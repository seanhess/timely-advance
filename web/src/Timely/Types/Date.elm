module Timely.Types.Date exposing (Date, current, decodeDate, empty, encodeDate, formatDate)

import Date
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import String
import Task
import Time exposing (Month(..), Zone)


type alias Date =
    Date.Date


decodeDate : Decoder Date
decodeDate =
    Iso8601.decoder
        |> Decode.map (Date.fromPosix Time.utc)


encodeDate : Date -> Value
encodeDate d =
    Encode.string <| Date.toIsoString d


formatDate : Date -> String
formatDate =
    Date.format "MMMM ddd, y"


empty : Date
empty =
    -- Only use this when using Date.current
    Date.fromCalendarDate 1 Jan 2000


current : (Date -> msg) -> Cmd msg
current onTime =
    Date.today |> Task.perform onTime



-- formatTime : Date -> String
-- formatTime time =
--     [ Time.toHour Time.utc time, Time.toMinute Time.utc time, Time.toSecond Time.utc time ]
--         |> List.map (String.padLeft 2 '0' << String.fromInt)
--         |> String.join ":"
