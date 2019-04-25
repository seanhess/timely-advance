module Test.Dates where

import Data.Maybe as Maybe
import qualified Data.Time.Format      as Time


parseTime :: Time.ParseTime a => String -> a
parseTime a = Maybe.fromMaybe (error a) $ Time.parseTimeM True Time.defaultTimeLocale (Time.iso8601DateFormat (Just "%H:%M:%S")) a

parseDay :: Time.ParseTime a => String -> a
parseDay a = Maybe.fromMaybe (error a) $ Time.parseTimeM True Time.defaultTimeLocale (Time.iso8601DateFormat Nothing) a

day :: Time.ParseTime a => String -> a
day = parseDay

time :: Time.ParseTime a => String -> a
time = parseTime
