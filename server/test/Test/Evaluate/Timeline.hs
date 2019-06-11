{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate.Timeline where

import qualified Data.List                          as List
import qualified Data.Model.Money                   as Money
import           Data.Number.Abs                    (absolute)
import           Test.Dates                         (day)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Evaluate.Health.Budget      (Budget (Budget))
import qualified Timely.Evaluate.Health.Event       as Event
import           Timely.Evaluate.Health.Timeline    as Timeline
import qualified Timely.Evaluate.Health.Transaction as Trans
import           Timely.Evaluate.Schedule           (DayOfMonth (DayOfMonth), DayOfWeek (..), Schedule (Monthly, Weekly))


specTimeline = do
  ts <- runTests tests
  defaultMain $ testGroup "tests" ts


-- I want to do some high-level tests here
tests :: Tests ()
tests = do
  group "trans" testTrans
  group "events" testEvents
  group "lowestBalance" testLowestBalance



testLowestBalance :: Tests ()
testLowestBalance = do
  let bal = Money.fromFloat 100
  let evnts = \bal d ps bs -> allEvents bal $ allTransactions d ps bs
  let zero = Money.fromFloat 0
  let lowest = \b d ps bs -> lowestBalance b (evnts b d ps bs)

  group "single event" $ do
    test "should be bal - rentAmt" $ do
      lowest bal (day "2019-03-02") [] [rent5] @?= bal - rentAmt

    test "should be bal" $ do
      lowest bal (day "2019-03-02") [pay5] [] @?= bal

  group "rent, then pay" $ do
    test "should be low" $ do
      lowest bal (day "2019-03-02") [pay1] [rent5] @?= bal - rentAmt


  group "pay, then rent" $ do
    test "bills are bigger than pay, lowest after bills" $ do
      lowest bal (day "2019-03-06") [pay1] [rent5, rent5, rent5] @?= sum [bal, payAmt, -rentAmt, -rentAmt, -rentAmt]


  group "pay is bigger than bills, lowest is balance" $ do
    test "should be ok" $ do
      let low = Money.fromFloat 100
      lowest low (day "2019-03-06") [pay1] [rent5] @?= low


  group "monthly rent, montly pay" $ do

    test "just paid, due soon" $ do
      lowest payAmt (day "2019-03-02") [pay1] [rent5] @?= sum [payAmt, -rentAmt]

    test "payday, due soon, paycheck landed" $ do
      lowest payAmt (day "2019-03-01") [pay1] [rent5] @?= sum [payAmt, -rentAmt]

    test "payday, due soon, paycheck not landed" $ do
      lowest zero (day "2019-03-01") [pay1] [rent5] @?= -rentAmt

    test "just before payday" $ do
      lowest bal (day "2019-02-28") [pay1] [rent5] @?= bal


  group "monthly rent, weekly paychecks" $ do
    test "one more paycheck" $ do
      -- 03-01 = Friday, 03-04 = Monday
      lowest bal (day "2019-03-01") [payMon] [rent5] @?= sum [bal, payAmtWeek, -rentAmt]

    test "no more paychecks" $ do
      -- 03-04 = Monday
      lowest bal (day "2019-03-04") [payMon] [rent5] @?= sum [bal, -rentAmt]

    test "mid month" $ do
      -- 3 more paychecks but balance is lowest
      lowest bal (day "2019-02-15") [payMon] [rent5] @?= bal

    test "just paid it" $ do
      -- 4 paychecks before rent, balance is lowest
      lowest bal (day "2019-02-06") [payMon] [rent5] @?= bal

    test "due today" $ do
      -- We ignore the next bill, since it's today
      lowest bal (day "2019-03-05") [payMon] [rent5] @?= bal


  group "weekly bill, monthly paycheck" $ do
    test "just paid" $ do
      -- there are 4 bills until the next paycheck + same day = 5
      lowest payAmt (day "2019-03-02") [pay1] [billMon] @?= payAmt - (5*billAmtWeek)

    test "week later" $ do
      lowest payAmt (day "2019-03-09") [pay1] [billMon] @?= payAmt - (4*billAmtWeek)

    test "one bill, then paycheck" $ do
      lowest bal (day "2019-03-30") [pay1] [billMon] @?= bal - billAmtWeek




testEvents :: Tests ()
testEvents = do
  let zero = Money.fromFloat 0
  let evnts = \bal d ps bs -> allEvents bal $ allTransactions d ps bs
  let balances = \bal d ps bs -> List.map Event.balance $ evnts bal d ps bs

  group "single event" $ do
    test "balance should be pay amount" $ do
      (balances zero (day "2019-03-01") [pay5] []) @?= [payAmt]

    test "balance should be debt amount" $ do
      (balances zero (day "2019-03-01") [] [rent5]) @?= [-rentAmt]


  group "rent, then pay" $ do
    let es = evnts zero (day "2019-03-02") [pay1] [rent5]

    test "order correct" $ do
      (List.map (Trans.name . Event.transaction) es) @?= ["rent", "paycheck"]

    test "balances" $ do
      (List.map Event.balance es) @?= [-rentAmt, -rentAmt + payAmt]


  group "pay, then rent" $ do
    let es = evnts zero (day "2019-03-06") [pay1] [rent5]

    test "order correct" $ do
      (List.map (Trans.name . Event.transaction) es) @?= ["paycheck", "rent"]

    test "balances" $ do
      (List.map Event.balance es) @?= [payAmt, payAmt - rentAmt]


  group "same day means bills happen first" $ do
    let es = evnts zero (day "2019-03-01") [pay5] [rent5]

    test "order correct" $ do
      (List.map (Trans.name . Event.transaction) es) @?= ["rent", "paycheck"]

    test "balances" $ do
      (List.map Event.balance es) @?= [-rentAmt, -rentAmt + payAmt]


  group "uses initial balance" $ do
    let bal = Money.fromFloat 1000
    let es = evnts bal (day "2019-03-02") [pay1] [rent5]

    test "balances" $ do
      (List.map Event.balance es) @?= [bal - rentAmt, bal - rentAmt + payAmt]


  group "same day" $ do
    let ts = evnts zero (day "2019-03-01") [pay5] [rent5]

    test "should be on date" $ do
      List.map (Trans.date . Event.transaction) ts @?= [day "2019-03-05", day "2019-03-05"]

    test "expense first" $ do
      List.map (Trans.name . Event.transaction) ts @?= ["rent", "paycheck"]

    test "should have amounts" $ do
      List.map (Trans.amount . Event.transaction) ts @?= [-rentAmt, payAmt]

  group "between pay and rent" $ do
    let ts = evnts zero (day "2019-03-02") [pay1] [rent5]

    test "dates" $ do
      List.map (Trans.date . Event.transaction) ts @?= [day "2019-03-05", day "2019-04-01"]

    test "rent, then pay" $ do
      List.map (Trans.name . Event.transaction) ts @?= ["rent", "paycheck"]


  group "after both" $ do
    let ts = evnts zero (day "2019-03-05") [pay1] [rent5]

    test "dates" $ do
      List.map (Trans.date . Event.transaction) ts @?= [day "2019-04-01", day "2019-04-05"]

    test "pay, then rent" $ do
      List.map (Trans.name . Event.transaction) ts @?= ["paycheck","rent"]


  group "happens today" $ do
    let ts = evnts zero (day "2019-03-05") [rent5] [rent5]

    -- Scenario: today is rent day, we just bought a hamburger, but rent hasn't hit yet. We can't issue an advance in time to help them out with rent, so we won't include it the calculation.
    -- Scenario: today is rent day, and rent just hit. It's factored into the balance.
    test "should be next month" $ do
      List.map (Trans.date . Event.transaction) ts @?= [day "2019-04-05", day "2019-04-05"]







testTrans :: Tests ()
testTrans = do

  group "monthly income" $ do
    let ts = credits (day "2019-03-01") pay5

    test "should include one paycheck" $ do
      length ts @?= 1
    test "should be on paydate" $ do
      Trans.date (head ts) @?= day "2019-03-05"
    test "should be the amount" $ do
      Trans.amount (head ts) @?= payAmt

  group "next month's income" $ do
    let ts = credits (day "2019-03-05") pay5

    test "should include one paycheck" $ do
      length ts @?= 1
    test "should be on paydate" $ do
      Trans.date (head ts) @?= day "2019-04-05"
    test "should be the amount" $ do
      Trans.amount (head ts) @?= payAmt

  group "monthly bill" $ do
    let ts = debits (day "2019-03-01") rent5

    test "should include one paycheck" $ do
      length ts @?= 1

    test "should be on paydate" $ do
      Trans.date (head ts) @?= day "2019-03-05"

    test "should be the amount" $ do
      Trans.amount (head ts) @?= (-rentAmt)










rent5 = Budget "rent" (Monthly (DayOfMonth 5)) (absolute rentAmt)
pay5 = Budget "paycheck" (Monthly (DayOfMonth 5)) (absolute payAmt)
pay1 = Budget "paycheck" (Monthly (DayOfMonth 1)) (absolute payAmt)
payMon = Budget "paycheck mon" (Weekly Monday) (absolute payAmtWeek)
billMon = Budget "bill mon" (Weekly Monday) (absolute billAmtWeek)

rentAmt = Money.fromFloat 1000
payAmt = Money.fromFloat 2000
payAmtWeek = Money.fromFloat 500
billAmtWeek = Money.fromFloat 100
