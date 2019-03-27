module Timely.Evaluate.Schedule
  ( Schedule(..)
  , DayOfMonth(..)
  , DayOfWeek(..)
  , Biweek(..)
  , biweek
  , schedule
  , last
  , next
  , until
  , dayOfWeek
  , dayOfMonth
  , nextWeekday
  ) where

-- import Debug.Trace (traceShow)
import           Control.Applicative                 ((<|>))
import           Control.Monad                       (guard)
import           Data.List                           (group, sortBy, sortOn)
import qualified Data.List                           as List
import           Data.Maybe                          (listToMaybe)
import           Data.Ord                            (Down (..), comparing)
import           Data.Time.Calendar                  (Day (..), addGregorianMonthsClip, fromGregorian, toGregorian)
import qualified Data.Time.Calendar                  as Day
import           Prelude                             hiding (last, until)
import           Timely.Evaluate.Schedule.DayOfMonth (DayOfMonth (..))
import           Timely.Evaluate.Schedule.DayOfWeek  (DayOfWeek (..))






data Schedule
    = Weekly      DayOfWeek
    | Biweekly    DayOfWeek Biweek
    | Monthly     DayOfMonth
    | Semimonthly DayOfMonth DayOfMonth
    deriving (Eq, Show)


-- | There are forever only "A" and "B" weeks. No matter the leap year, etc. It's helpful to put a repeating event in your calendar when testing.
-- | weeks start on Monday
data Biweek
    = A -- starts Monday, Jan 1, 2018
    | B -- starts Monday, Jan 8, 2018
    deriving (Eq, Show)



biweek :: Day -> Biweek
biweek d =
  let a    = fromGregorian 2018 01 01
      diff = abs (Day.diffDays d a)
  in if diff `mod` 14 < 7
    then A
    else B




schedule :: [Day] -> Maybe Schedule
schedule ds = do
  let mgs = monthdayGroups ds
      wgs = weekdayGroups ds
  wg  <- listToMaybe wgs
  let int = average $ intervals ds

  semimonthly mgs
    <|> biweekly int wg ds
    <|> monthly mgs
    <|> weekly int wg
    <|> monthlyDrift ds

  where
    atLeastTwo :: [a] -> Maybe a
    atLeastTwo (a:_:_) = Just a
    atLeastTwo _       = Nothing

    -- Note: Maybe calls fail if a pattern match fails (=Nothing)
    semimonthly mgs = do
      [mg1, mg2] <- pure $ take 2 mgs
      m1 <- atLeastTwo mg1
      m2 <- atLeastTwo mg2
      pure $ Semimonthly m1 m2

    biweekly int wg ds = do
      let d = maximum ds
      w <- atLeastTwo wg
      guard $ 13 < int && int < 15
      pure $ Biweekly w (biweek d)

    weekly int wg = do
      w <- atLeastTwo wg
      guard $ 6 < int && int < 8
      pure $ Weekly w

    monthly :: [[DayOfMonth]] -> Maybe Schedule
    monthly mgs = do
      mg <- listToMaybe mgs
      m <- atLeastTwo mg
      guard $ length mg > sum (map length $ drop 1 mgs)
      pure $ Monthly m

    monthlyDrift ds = do
      -- only two samples, and the dates are off by 1
      [DayOfMonth d1, DayOfMonth d2] <- pure $ map dayOfMonth ds
      guard $ abs (d1 - d2) == 1
      pure $ Monthly $ DayOfMonth d1



-- if we have a regular weekday, but irregular dates
weekdayGroups :: [Day] -> [[DayOfWeek]]
weekdayGroups = toGroups . sortOn fromEnum . map dayOfWeek


monthdayGroups :: [Day] -> [[DayOfMonth]]
monthdayGroups = toGroups . sortOn day . map dayOfMonth


toGroups :: Eq a => [a] -> [[a]]
toGroups = sortBy (comparing (Down . length)) . group




intervals :: [Day] -> [Integer]
intervals ds =
  List.zipWith interval (drop 1 ds) ds
  where
    interval d1 d2 = abs $ Day.diffDays d1 d2



average :: [Integer] -> Float
average xs = sum (map fromIntegral xs) / fromIntegral (length xs)

















type DiffDays = Integer


-- Last -------------------------------------
last :: Schedule -> Day -> Day
last schedule today =
  -- go back more than a month (the longest interval) and find the last one
  let dates = until today schedule (Day.addDays (-50) today)
  in List.last dates


-- Next -------------------------------------


next :: Schedule -> Day -> Day
next (Weekly dow) today = nextWeekday dow today
next (Biweekly d b) today =
  let first = nextWeekday d today
  in if biweek first == b
    then first
    else nextWeekday d (Day.addDays 7 today)
next (Monthly d) today  = nextMonthday d today
next (Semimonthly d1 d2) today = min
  (nextMonthday d1 today)
  (nextMonthday d2 today)


until :: Day -> Schedule -> Day -> [Day]
until end schedule today =
  List.takeWhile (< end) $ drop 1 $ List.iterate (next schedule) today



nextMonthday :: DayOfMonth -> Day -> Day
nextMonthday (DayOfMonth next) today =
  let (y,m,d) = toGregorian today
      months  = if next <= d then 1 else 0
  in addGregorianMonthsClip months $ fromGregorian y m next




nextWeekday :: DayOfWeek -> Day -> Day
nextWeekday next today =
  let curr = dayOfWeek today
      diff = nextWeek $ diffWeekdays curr next
  in Day.addDays diff today


diffWeekdays :: DayOfWeek -> DayOfWeek -> DiffDays
diffWeekdays curr next =
    fromIntegral (fromEnum next - fromEnum curr)


nextWeek :: DiffDays -> DiffDays
nextWeek d
    | d <= 0 = d + 7
    | otherwise = d











dayOfWeek :: Day -> DayOfWeek
dayOfWeek (ModifiedJulianDay d) = toEnum $ fromInteger $ d + 3

dayOfMonth :: Day -> DayOfMonth
dayOfMonth d =
  let (_, _, day) = toGregorian d
  in DayOfMonth day
