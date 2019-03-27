{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate.Schedule where

import           Test.Dates       (parseDay)
import           Test.Tasty.HUnit
import           Test.Tasty
import           Test.Tasty.Monad
import           Timely.Evaluate.Schedule as Schedule
import           Timely.Evaluate.Schedule.DayOfMonth (DayOfMonth (..))
-- import           Timely.Evaluate.History          as History hiding (expenses, income)


specSchedule = defaultMain $ testGroup "tests" $ runTests tests

tests :: Tests ()
tests = do
  group "schedule" testSchedule
  group "biweek" testBiweek
  group "next" testNext
  group "last" testLast
  group "until" testUntil



testBiweek :: Tests ()
testBiweek = do
  group "original" $ do
    test "A Mon" $ biweek (parseDay "2018-01-01") @?= A
    test "A Wed" $ biweek (parseDay "2018-01-03") @?= A
    test "A Sun" $ biweek (parseDay "2018-01-07") @?= A
    test "B Mon" $ biweek (parseDay "2018-01-08") @?= B
    test "B Wed" $ biweek (parseDay "2018-01-10") @?= B
    test "B Sun" $ biweek (parseDay "2018-01-14") @?= B
    test "A Mon +1" $ biweek (parseDay "2018-01-15") @?= A

  group "now" $ do
    test "week 1 beg A" $ biweek (parseDay "2019-01-01") @?= A
    test "week 1 end A" $ biweek (parseDay "2019-01-06") @?= A
    test "week 2 beg B" $ biweek (parseDay "2019-01-07") @?= B
    test "week 2 end B" $ biweek (parseDay "2019-01-12") @?= B
    test "week 3 beg A" $ biweek (parseDay "2019-01-14") @?= A




testLast :: Tests ()
testLast = do
  group "weekly" $ do
    let today = parseDay "2019-03-01" -- Friday

    test "last monday" $ do
      Schedule.last (Weekly Monday) today @?= parseDay "2019-02-25"

    test "last friday" $ do
      Schedule.last (Weekly Friday) today @?= parseDay "2019-02-22"

  group "monthly" $ do
    let today = parseDay "2019-03-01" -- Friday
    test "last 1st" $ do
      Schedule.last (Monthly (DayOfMonth 1)) today @?= parseDay "2019-02-01"

    test "last 28th" $ do
      Schedule.last (Monthly (DayOfMonth 28)) today @?= parseDay "2019-02-28"

    test "last month" $ do
      Schedule.last (Monthly (DayOfMonth 5)) (parseDay "2019-02-05") @?= parseDay "2019-01-05"


  group "semimonthly" $ do
    let today = parseDay "2019-03-15" -- Friday
    test "last 5th" $ do
      Schedule.last (Semimonthly (DayOfMonth 1) (DayOfMonth 5)) today @?= parseDay "2019-03-05"

    test "last 5th/20" $ do
      Schedule.last (Semimonthly (DayOfMonth 5) (DayOfMonth 20)) today @?= parseDay "2019-03-05"

    test "last 20" $ do
      Schedule.last (Semimonthly (DayOfMonth 15) (DayOfMonth 20)) today @?= parseDay "2019-02-20"

  group "biweekly" $ do
    let today = parseDay "2019-03-15" -- Friday A
    test "last monday A" $ do
      Schedule.last (Biweekly Monday A) today @?= parseDay "2019-03-11"

    test "last monday B" $ do
      Schedule.last (Biweekly Monday B) today @?= parseDay "2019-03-04"

    test "last friday A" $ do
      Schedule.last (Biweekly Friday A) today @?= parseDay "2019-03-01"

    test "last friday B" $ do
      Schedule.last (Biweekly Friday B) today @?= parseDay "2019-03-08"






testUntil :: Tests ()
testUntil = do
  let today = parseDay "2019-01-01" -- Tuesday
  test "4 weeks in january" $ do
    let dates = Schedule.until (parseDay "2019-02-01") (Weekly Monday) today
    length dates @?= 4
    head dates @?= Schedule.next (Weekly Monday) today
    head (drop 1 dates) @?= Schedule.next (Weekly Monday) (head dates)

  test "monthly" $ do
    let dates = Schedule.until (parseDay "2019-03-01") (Monthly (DayOfMonth 5)) today
    dates @?= [parseDay "2019-01-05", parseDay "2019-02-05"]

  test "should not include end date" $ do
    let dates = Schedule.until (parseDay "2019-02-01") (Monthly (DayOfMonth 1)) today
    dates @?= []


testNext :: Tests ()
testNext = do
  group "weekly" $ do
    let today = parseDay "2019-01-01" -- Tuesday

    test "next monday" $ do
      next (Weekly Monday) today @?= parseDay "2019-01-07"

    test "next tuesday" $ do
      next (Weekly Tuesday) today @?= parseDay "2019-01-08"

    test "next wed tomorrow" $ do
      next (Weekly Wednesday) today @?= parseDay "2019-01-02"


  group "monthly" $ do
    let today = parseDay "2019-01-12"
    test "next 2nd next month" $ do
      next (Monthly (DayOfMonth 2)) today @?= parseDay "2019-02-02"

    test "next 13th this month" $ do
      next (Monthly (DayOfMonth 13)) today @?= parseDay "2019-01-13"

    test "next 28th this month" $ do
      next (Monthly (DayOfMonth 28)) today @?= parseDay "2019-01-28"

    test "next 10th next month" $ do
      next (Monthly (DayOfMonth 10)) today @?= parseDay "2019-02-10"

    test "next 12th next month" $ do
      next (Monthly (DayOfMonth 12)) today @?= parseDay "2019-02-12"


  group "semimonthly" $ do
    let today = parseDay "2019-01-12"
    test "second date is earlier" $ do
      next (Semimonthly (DayOfMonth 5) (DayOfMonth 20)) today @?= parseDay "2019-01-20"

    test "first date is earlier" $ do
      next (Semimonthly (DayOfMonth 15) (DayOfMonth 30)) today @?= parseDay "2019-01-15"

    test "first date is next month" $ do
      next (Semimonthly (DayOfMonth 05) (DayOfMonth 11)) today @?= parseDay "2019-02-05"

    test "second date next month" $ do
      next (Semimonthly (DayOfMonth 10) (DayOfMonth 11)) today @?= parseDay "2019-02-10"


  group "biweekly" $ do
    let today = parseDay "2019-01-15" -- Tuesday, A

    test "next monday B" $ do
      next (Biweekly Monday B) today @?= parseDay "2019-01-21"

    test "next monday A" $ do
      next (Biweekly Monday A) today @?= parseDay "2019-01-28"

    test "next tuesday A" $ do
      next (Biweekly Tuesday A) today @?= parseDay "2019-01-29"

    test "next tuesday B" $ do
      next (Biweekly Tuesday B) today @?= parseDay "2019-01-22"

    test "next wednesday A" $ do
      next (Biweekly Wednesday A) today @?= parseDay "2019-01-16"

    test "next wednesday B" $ do
      next (Biweekly Wednesday B) today @?= parseDay "2019-01-23"



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
    test "14d" $ schedule [parseDay "2019-01-05", parseDay "2019-01-19"] @?= Just (Biweekly Saturday A)
    test "2x -1 nothing" $ schedule [parseDay "2019-01-05", parseDay "2019-01-18"] @?= Nothing
    test "3x -1 yes" $ schedule [parseDay "2019-01-01", parseDay "2019-01-14", parseDay "2019-01-29"] @?= Just (Biweekly Tuesday A)
    test "biweek B" $ schedule [parseDay "2019-01-10", parseDay "2019-01-24"] @?= Just (Biweekly Thursday B)



-- wait, semimonthly could be ANYTHING
  group "semimonthly" $ do
    test "repeating days" $ do
      schedule [parseDay "2019-01-05", parseDay "2019-01-20", parseDay "2019-02-05", parseDay "2019-02-20"] @?= Just (Semimonthly (DayOfMonth 05) (DayOfMonth 20))

    test "drifting days" $ do
      schedule [parseDay "2019-01-05", parseDay "2019-01-20", parseDay "2019-02-05", parseDay "2019-02-19", parseDay "2019-03-05", parseDay "2019-03-20"] @?= Just (Semimonthly (DayOfMonth 05) (DayOfMonth 20))

    test "drifting days not enough" $ do
      schedule [parseDay "2019-01-05", parseDay "2019-01-20", parseDay "2019-02-05", parseDay "2019-02-19"] @?= Nothing


