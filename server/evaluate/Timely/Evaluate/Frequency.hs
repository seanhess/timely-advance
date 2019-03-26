{-# LANGUAGE DeriveGeneric #-}
module Timely.Evaluate.Frequency where


import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)
import           Data.List (group, sort)
import Data.Maybe (listToMaybe)

data Frequency
    = Weekly
    | Biweekly
    | Monthly
    | Semimonthly
    deriving (Show, Eq, Generic)

instance ToJSON Frequency
instance FromJSON Frequency




-- can we identify a frequency?
-- if not, someone can be specific about how they use defaults
-- get the intervals
frequency :: [Day] -> Maybe Frequency
frequency ds = do
  int <- mainInterval $ intervals ds
  intervalToFrequency int


-- List the intervals between dates
-- if there's more than one, are they exact?
intervals :: Num a => [Day] -> [a]
intervals = undefined


mainInterval :: (Ord a, Eq a) => [a] -> Maybe a
mainInterval xs = do
  mxs <- listToMaybe $ group $ sort xs
  listToMaybe mxs








-- we don't know that semi monthly is divided by 15 days. Not helpful
intervalToFrequency :: Integer -> Maybe Frequency
intervalToFrequency n
  | n == 7  = Just Weekly
  | n == 14 = Just Biweekly
  -- | n == 15 = Just Semimonthly
  | n >= 28 = Just Monthly
  | otherwise = Nothing


average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)

-- fromInterval :: Float -> Maybe

-- scheduleFromInterval :: Float -> [Day] -> Maybe Schedule
-- scheduleFromInterval i ds
--   | i < 8.0  = Just $ scheduleWeekly ds
--   | i < 15.0 = Just $ scheduleBiweekly ds

-- scheduleWeekly :: [Day] -> Schedule
-- scheduleWeekly = undefined
