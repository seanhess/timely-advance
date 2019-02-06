module Timely.Advances.Collect where


import           Data.Time.Calendar (Day, addDays)
import           Data.Time.Clock    (UTCTime(..))





-- | Collect at Midnight UTC / 5pm MT / 7pm ET. Once it is midnight, we pick up all the stuff from the "current" (now previous) day
currentlyDue :: UTCTime -> Day
currentlyDue (UTCTime d _) = addDays (-1) d



