{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate.Schedule where

import           Test.Dates       (parseDay)
import           Test.Tasty.HUnit
import           Test.Tasty
import           Test.Tasty.Monad
import           Timely.Evaluate.Schedule (schedule, Schedule(..), DayOfMonth(..), DayOfWeek(..))
import           Timely.Evaluate.Frequency (frequency)
import qualified Timely.Evaluate.Frequency as Frequency
-- import           Timely.Evaluate.History          as History hiding (expenses, income)


specSchedule = defaultMain $ testGroup "tests" $ runTests tests

tests :: Tests ()
tests = do
  group "schedule" testSchedule
  -- group "frequency" testFrequency




-- wait, maybe I can use this to determine whether I think something IS a bill. By only returning a frequency if I'm pretty sure. The example in postico was 30 days, I think. The date slowly drifted. 

-- testFrequency :: Tests ()
-- testFrequency = do
--   group "outliers" $ do
--     test "empty" $ do
--       frequency [] @?= Nothing

--     test "only one date" $ do
--       frequency [parseDay "2019-01-03"] @?= Nothing


--   group "monthly" $ do
--     test "two dates" $ do
--       frequency [parseDay "2019-01-05", parseDay "2019-02-05"] @?= Just (Frequency.Monthly)

--     test "-1" $ do
--       frequency [parseDay "2019-01-05", parseDay "2019-02-04", parseDay "2019-02-05"] @?= Just (Frequency.Monthly)

--     test "+1" $ do
--       frequency [parseDay "2019-01-11", parseDay "2019-02-11", parseDay "2019-02-12"] @?= Just (Frequency.Monthly)

--     test "+1 with only two samples" $ do
--       frequency [parseDay "2019-01-11", parseDay "2019-02-12"] @?= Just (Frequency.Monthly)


--   group "weekly" $ do
--     test "2x if exactly 7d" $ frequency [parseDay "2019-01-05", parseDay "2019-01-12"] @?= Just (Frequency.Weekly)
--     test "2x nothing if 6d " $ frequency [parseDay "2019-01-05", parseDay "2019-01-11"] @?= Nothing
--     test "2x nothing if 8d" $ frequency [parseDay "2019-01-05", parseDay "2019-01-13"] @?= Nothing
--     test "3x found if two repeat -1" $ frequency [parseDay "2019-01-06", parseDay "2019-01-13", parseDay "2019-01-19"] @?= (Just $ Frequency.Weekly)
--     test "3x found if two repeat +1" $ frequency [parseDay "2019-01-04", parseDay "2019-01-12", parseDay "2019-01-18"] @?= (Just Frequency.Weekly)


--   group "biweekly" $ do
--     test "14d" $ frequency [parseDay "2019-01-05", parseDay "2019-01-19"] @?= Just (Frequency.Biweekly)
--     test "2x 13d nothing" $ frequency [parseDay "2019-01-05", parseDay "2019-01-18"] @?= Nothing
--     test "3x 13d yes" $ frequency [parseDay "2019-01-01", parseDay "2019-01-14", parseDay "2019-01-31"] @?= Just (Frequency.Biweekly)


-- -- wait, semimonthly could be ANYTHING
-- -- it's not necessarily an interval 
-- -- it's more that the dates repeat
-- -- well, a better question: do we have repeating days of the week?
-- -- or do we have repeating dates
--   group "semimonthly" $ do
--     test "repeating days" $ do
--       frequency [parseDay "2019-01-05", parseDay "2019-01-20", parseDay "2019-02-05", parseDay "2019-02-20"] @?= Just (Frequency.Semimonthly)

--     test "drifting days" $ do
--       frequency [parseDay "2019-01-05", parseDay "2019-01-20", parseDay "2019-02-05", parseDay "2019-02-19"] @?= Just (Frequency.Semimonthly)







testSchedule :: Tests ()
testSchedule = do
  group "outliers" $ do
    test "empty" $ do
      schedule [] @?= Nothing

    test "only one date" $ do
      schedule [parseDay "2019-01-03"] @?= Nothing


  group "monthly" $ do
    test "two dates" $ do
      schedule [parseDay "2019-01-05", parseDay "2019-02-05"] @?= Just (Monthly (DayOfMonth 5))

    test "-1" $ do
      schedule [parseDay "2019-01-05", parseDay "2019-02-04", parseDay "2019-02-05"] @?= Just (Monthly (DayOfMonth 5))

    test "+1" $ do
      schedule [parseDay "2019-01-11", parseDay "2019-02-11", parseDay "2019-02-12"] @?= Just (Monthly (DayOfMonth 11))

    test "+1 with only two samples" $ do
      schedule [parseDay "2019-01-11", parseDay "2019-02-12"] @?= Just (Monthly (DayOfMonth 11))

    test "february has 4 weeks, shouldn't pick weekly" $ do
      schedule [parseDay "2019-02-04", parseDay "2019-03-04"] @?= Just (Monthly (DayOfMonth 4))


  group "weekly" $ do
    test "2x if exactly 7d" $ schedule [parseDay "2019-01-05", parseDay "2019-01-12"] @?= Just (Weekly Saturday)
    test "2x nothing if 6d " $ schedule [parseDay "2019-01-05", parseDay "2019-01-11"] @?= Nothing
    test "2x nothing if 8d" $ schedule [parseDay "2019-01-05", parseDay "2019-01-13"] @?= Nothing
    test "3x found if two repeat -1" $ schedule [parseDay "2019-01-06", parseDay "2019-01-13", parseDay "2019-01-19"] @?= (Just $ Weekly Sunday)
    test "3x found if two repeat +1" $ schedule [parseDay "2019-01-04", parseDay "2019-01-12", parseDay "2019-01-18"] @?= (Just $ Weekly Friday)


  group "biweekly" $ do
    test "14d" $ schedule [parseDay "2019-01-05", parseDay "2019-01-19"] @?= Just (Biweekly Saturday)
    test "2x -1 nothing" $ schedule [parseDay "2019-01-05", parseDay "2019-01-18"] @?= Nothing
    test "3x -1 yes" $ schedule [parseDay "2019-01-01", parseDay "2019-01-14", parseDay "2019-01-29"] @?= Just (Biweekly Tuesday)


-- wait, semimonthly could be ANYTHING
  group "semimonthly" $ do
    test "repeating days" $ do
      schedule [parseDay "2019-01-05", parseDay "2019-01-20", parseDay "2019-02-05", parseDay "2019-02-20"] @?= Just (Semimonthly (DayOfMonth 05) (DayOfMonth 20))

    test "drifting days" $ do
      schedule [parseDay "2019-01-05", parseDay "2019-01-20", parseDay "2019-02-05", parseDay "2019-02-19", parseDay "2019-03-05", parseDay "2019-03-20"] @?= Just (Semimonthly (DayOfMonth 05) (DayOfMonth 20))

    test "drifting days not enough" $ do
      schedule [parseDay "2019-01-05", parseDay "2019-01-20", parseDay "2019-02-05", parseDay "2019-02-19"] @?= Nothing


