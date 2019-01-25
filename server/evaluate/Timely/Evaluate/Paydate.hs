module Timely.Evaluate.Paydate
    ( DayOfWeek(..)
    , dayOfWeek
    , mon, tue, wed, thu, fri, sat, sun
    , DayOfMonth(..)

    , PayFrequency (..)
    , frequency
    , next
    ) where

import           Data.Time.Calendar (Day)
import qualified Data.Time.Calendar as Day
import qualified Data.Time.Calendar.WeekDate as Week






data PayFrequency
    = Weekly    DayOfWeek
    | Biweekly  DayOfWeek
    | Monthly   DayOfMonth
    | Bimonthly DayOfMonth DayOfMonth


type Transaction = ()


-- TODO
-- mocked: payday is always next monday
frequency :: [Transaction] -> PayFrequency
frequency _ = Weekly mon









type DiffDays = Integer

-- TODO
-- mocked: pay frequency is always weekly
next :: PayFrequency -> Day -> Day
next (Weekly dow) today = nextWeekday dow today
next _            today = nextWeekday mon today


nextWeekday :: DayOfWeek -> Day -> Day
nextWeekday next today =
  let curr = toDayOfWeek today
      diff = nextWeek $ diffWeekdays curr next
  in Day.addDays diff today


-- not quite right... should always be the future
diffWeekdays :: DayOfWeek -> DayOfWeek -> DiffDays
diffWeekdays (DayOfWeek curr) (DayOfWeek next) =
    fromIntegral (next - curr)





nextWeek :: DiffDays -> DiffDays
nextWeek d
    | d <= 0 = d + 7
    | otherwise = d



-- 7 + 2 - 1 = 
-- it should just be 1
-- oh but if it's 0 it should be 7


toDayOfWeek :: Day -> DayOfWeek
toDayOfWeek d =
    let (_, _, dow) = Week.toWeekDate d
    in DayOfWeek dow





newtype DayOfMonth = DayOfMonth Int

-- Monday = 1
newtype DayOfWeek  = DayOfWeek Int

dayOfWeek n
    | n < 1 = Nothing
    | n > 7 = Nothing
    | otherwise = Just $ DayOfWeek n

mon = DayOfWeek 1
tue = DayOfWeek 2
wed = DayOfWeek 3
thu = DayOfWeek 4
fri = DayOfWeek 5
sat = DayOfWeek 6
sun = DayOfWeek 7
