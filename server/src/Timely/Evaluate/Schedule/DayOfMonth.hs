{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Timely.Evaluate.Schedule.DayOfMonth where


import Data.Aeson (ToJSON, FromJSON)

-- Valid days are 1-28 (not all months have a 29, 30, 31)
newtype DayOfMonth = DayOfMonth { day :: Int }
  deriving (Show, Eq, ToJSON, FromJSON)

fromInt :: Int -> Maybe DayOfMonth
fromInt i
  | 1 <= i && i <= 28 = Just $ DayOfMonth i
  | otherwise = Nothing

