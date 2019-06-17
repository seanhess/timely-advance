{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate.Timeline where

import Data.Function                   ((&))
import Data.Model.Money                as Money (money)
import Data.Number.Abs                 (Abs (value), absolute)
import Data.Time.Calendar              (Day)
import Test.Dates                      (day)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Monad
import Timely.Evaluate.Health.Budget   (Budget (Budget), Scheduled(Scheduled))
import Timely.Evaluate.Health.Daily    as Daily (Daily (..), DailyBalance (..))
import Timely.Evaluate.Health.Timeline as Timeline
import Timely.Evaluate.Schedule        (DayOfMonth (DayOfMonth), DayOfWeek (..), Schedule (Monthly, Weekly))


specTimeline = do
  ts <- runTests tests
  defaultMain $ testGroup "tests" ts


-- I want to do some high-level tests here
tests :: Tests ()
tests = do
  group "timeline" testTimeline
  group "daily balance" testDailyBalance
  group "lowestBalance" testLowestBalance



testTimeline :: Tests ()
testTimeline = do
  group "dates" $ do
    test "one day if the same" $ do
      let ![d] = timeline day1 day1 spend0 []
      date d @?= day1

    test "no days if end < start" $ do
      timeline day2 day1 spend0 [] @?= []

    test "two days" $ do
      fmap date (timeline day1 day2 spend0 []) @?= [day1, day2]

  group "bills" $ do
    test "bill tomorrow" $ do
      let ![_, d5] = timeline day4 day5 spend0 [rent5]
      date d5     @?= day5
      spending d5 @?= spend0
      bills d5    @?= [rent5]
      dailyTotal d5 @?= absolute rentAmt

    test "two bills on the 5th" $ do
      -- 3-05 is Tuesday
      let billT = Budget "bill" (Weekly Tuesday) (absolute billAmtWeek)
      let ![_, d5] = timeline day4 day5 spend0 [rent5, billT]
      date d5     @?= day5
      bills d5    @?= [rent5, billT]
      dailyTotal d5 @?= (absolute $ billAmtWeek + rentAmt)

    test "two bills, week and 5th" $ do
      -- 3-04 is Monday
      let billM = Budget "bill" (Weekly Monday) (absolute billAmtWeek)
      let ![_, d4, d5] = timeline day3 day5 spend0 [rent5, billM]
      date d4 @?= day4
      date d5 @?= day5
      dailyTotal d4 @?= absolute billAmtWeek
      dailyTotal d5 @?= absolute rentAmt


  group "totals" $ do
    test "daily spending, no bills" $ do
      fmap spending (timeline day2 day3 spend30 []) @?= [spend30, spend30]

    test "daily spending, with bills" $ do
      fmap spending (timeline day4 day5 spend30 [rent5]) @?= [spend30, spend30]

    test "daily total, no bills" $ do
      fmap dailyTotal (timeline day4 day5 spend30 [rent5]) @?= [spend30, absolute (value spend30 + rentAmt)]

    test "daily total, no spend" $ do
      fmap dailyTotal (timeline day4 day5 spend0 [rent5]) @?= [absolute $ money 0, absolute rentAmt]


  group "totalSpending" $ do
    test "multiple days of spending, no bills" $ do
      totalSpending (timeline day1 day5 spend30 []) @?= absolute ((value spend30) * 5)

    test "single bill" $ do
      totalSpending (timeline day1 day5 spend0 [rent5]) @?= spend0

    test "multiple bills" $ do
      totalSpending (timeline day1 day5 spend0 [billMon, rent5]) @?= spend0

    test "multiple bills and spending" $ do
      totalSpending (timeline day1 day5 spend30 [billMon, rent5]) @?= (absolute $ (value spend30) * 5)

  group "billsDue" $ do
    test "no bills" $ do
      billsDue (timeline day1 day5 spend0 []) @?= []

    test "one bill" $ do
      billsDue (timeline day1 day5 spend0 [rent5]) @?= [Scheduled rent5 day5]

    test "two bills" $ do
      billsDue (timeline day1 day5 spend0 [rent5, billMon]) @?=
         [ Scheduled billMon day4
         , Scheduled rent5 day5
         ]





testDailyBalance :: Tests ()
testDailyBalance = do
  let bal = money 100

  test "empty" $ do
    dailyBalances (money 100) [] @?= []

  test "no spend, no bills" $ do
    fmap balance (dailyBalances bal [Daily day1 spend0 []]) @?= [bal]

  test "spending" $ do
    fmap balance (dailyBalances bal [Daily day1 spend30 []]) @?= [bal - value spend30]

  test "bill" $ do
    fmap balance (dailyBalances bal [Daily day5 spend0 [billMon]]) @?= [bal - billAmtWeek]

  test "spending + bill" $ do
    fmap balance (dailyBalances bal [Daily day5 spend30 [billMon]]) @?= [bal - billAmtWeek - value spend30]

  test "multiple days" $ do
    let ds = [ Daily day4 spend30 [billMon]
             , Daily day5 spend30 [rent5]
             , Daily day6 spend30 []
             ]
        b1 = bal - billAmtWeek - value spend30
        b2 = b1 - rentAmt - value spend30
        b3 = b2 - value spend30
    fmap balance (dailyBalances bal ds) @?= [b1, b2, b3]




testLowestBalance :: Tests ()
testLowestBalance = do
  let bal = money 100.00
  let minbal = \start end spend bills balance -> timeline start end spend bills & dailyBalances balance & minimumBalance balance

  -- let evnts = \bal d ps bs -> allEvents bal $ allTransactions d ps bs
  -- let lowest = \b d ps bs -> lowestBalance b (evnts b d ps bs)

  test "empty daily balance" $ do
    minimumBalance bal [] @?= bal

  test "high daily balance" $ do
    minimumBalance bal [DailyBalance undefined (money 200)] @?= bal

  test "small daily balance" $ do
    minimumBalance bal [DailyBalance undefined (money 50)] @?= money 50

  test "single bill, no spending" $ do
    minbal day1 day10 spend0 [rent5] bal @?= bal - rentAmt

  test "no bills, no spending" $ do
    minbal day1 day10 spend0 [] bal @?= bal

  test "no bills, spending" $ do
    minbal day1 day2 spend30 [] bal @?= bal - (value spend30 * 2)

  test "bills and spending" $ do
    minbal day4 day5 spend30 [rent5] bal @?= bal - (value spend30 * 2) - rentAmt

  test "multipe bills and spending" $ do
    minbal day1 day5 spend30 [rent5, billMon] bal @?= bal - (value spend30 * 5) - rentAmt - billAmtWeek

  test "same day skips due date" $ do
    minbal day5 day6 spend30 [rent5] bal @?= bal - (value spend30 * 2)



day1 = day "2019-03-01" :: Day
day2 = day "2019-03-02" :: Day
day3 = day "2019-03-03" :: Day
day4 = day "2019-03-04" :: Day
day5 = day "2019-03-05" :: Day
day6 = day "2019-03-06" :: Day
day10 = day "2019-03-10" :: Day


spend0  = absolute $ money 0.00
spend30 = absolute $ money 30.00

rent5 = Budget "rent" (Monthly (DayOfMonth 5)) (absolute rentAmt)
pay5 = Budget "paycheck" (Monthly (DayOfMonth 5)) (absolute payAmt)
pay1 = Budget "paycheck" (Monthly (DayOfMonth 1)) (absolute payAmt)
payMon = Budget "paycheck mon" (Weekly Monday) (absolute payAmtWeek)
billMon = Budget "bill mon" (Weekly Monday) (absolute billAmtWeek)

-- Tuesday, Mar 5th
billTue = Budget "bill tue" (Weekly Tuesday) (absolute billAmtWeek)

rentAmt = money 1000
payAmt = money 2000
payAmtWeek = money 500
billAmtWeek = money 100
