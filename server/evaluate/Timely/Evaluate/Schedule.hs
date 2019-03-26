module Timely.Evaluate.Schedule where

import           Data.Time.Calendar               (Day (..), toGregorian)
import qualified Data.Time.Calendar               as Day
import           Timely.AccountStore.Transactions (Transaction (..))
import Control.Monad (guard)
import qualified Timely.Evaluate.Frequency        as Frequency
import           Data.Ord                  (Down (..), comparing)
import Data.List (sortOn, group, sortBy)
import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import Control.Applicative ((<|>))
-- import qualified Data.Time.Calendar.WeekDate as Week






data Schedule
    = Weekly      DayOfWeek
    | Biweekly    DayOfWeek
    | Monthly     DayOfMonth
    | Semimonthly DayOfMonth DayOfMonth
    deriving (Eq, Show)



schedule :: [Day] -> Maybe Schedule
schedule ds = do
  let mgs = monthdayGroups ds
      wgs = weekdayGroups ds
  mg  <- listToMaybe mgs
  wg  <- listToMaybe wgs
  let int = average $ intervals ds

  semimonthly mgs
    <|> biweekly int wg
    <|> monthly mgs
    <|> weekly int wg
    <|> monthlyDrift ds

  where
    atLeastTwo :: [a] -> Maybe a
    atLeastTwo (a:_:_) = Just a
    atLeastTwo _ = Nothing

    isStrong l = length l > 1

    -- Note: Maybe calls fail if a pattern match fails (=Nothing)
    semimonthly mgs = do
      [mg1, mg2] <- pure $ take 2 mgs
      m1 <- atLeastTwo mg1
      m2 <- atLeastTwo mg2
      pure $ Semimonthly m1 m2

    biweekly int wg = do
      w <- atLeastTwo wg
      guard $ 13 < int && int < 15
      pure $ Biweekly w

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

-- TODO
-- mocked: pay frequency is always weekly
next :: Schedule -> Day -> Day
next (Weekly dow) today = nextWeekday dow today
next _            today = nextWeekday Monday today


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







newtype DayOfMonth = DayOfMonth { day :: Int }
  deriving (Show, Eq)



-- Copied from latest Data.Time.Calendar (which isn't on stackage yet)
data DayOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Show, Read)

-- | \"Circular\", so for example @[Tuesday ..]@ gives an endless sequence.
-- Also: 'fromEnum' gives [1 .. 7] for [Monday .. Sunday], and 'toEnum' performs mod 7 to give a cycle of days.
instance Enum DayOfWeek where
    toEnum i =
        case mod i 7 of
            0 -> Sunday
            1 -> Monday
            2 -> Tuesday
            3 -> Wednesday
            4 -> Thursday
            5 -> Friday
            _ -> Saturday
    fromEnum Monday    = 1
    fromEnum Tuesday   = 2
    fromEnum Wednesday = 3
    fromEnum Thursday  = 4
    fromEnum Friday    = 5
    fromEnum Saturday  = 6
    fromEnum Sunday    = 7
    enumFromTo wd1 wd2
        | wd1 == wd2 = [wd1]
    enumFromTo wd1 wd2 = wd1 : enumFromTo (succ wd1) wd2
    enumFromThenTo wd1 wd2 wd3
        | wd2 == wd3 = [wd1, wd2]
    enumFromThenTo wd1 wd2 wd3 = wd1 : enumFromThenTo wd2 (toEnum $ (2 * fromEnum wd2) - (fromEnum wd1)) wd3

dayOfWeek :: Day -> DayOfWeek
dayOfWeek (ModifiedJulianDay d) = toEnum $ fromInteger $ d + 3

dayOfMonth :: Day -> DayOfMonth
dayOfMonth d =
  let (_, _, day) = toGregorian d
  in DayOfMonth day
