{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate.Needed where

import           Data.Model.Money                   (Money)
import qualified Data.Model.Money                   as Money
import           Data.Time.Calendar                 (Day)
import           Test.Dates                         (parseDay)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Evaluate.Health.Needed      as Needed
import           Timely.Evaluate.Schedule           (DayOfMonth (..), DayOfWeek (..), Schedule (..))
import qualified Timely.Evaluate.Schedule           as Schedule


specNeeded = defaultMain $ testGroup "tests" $ runTests tests
-- specNeeded = defaultMain $ testGroup "tests" $ runTests testDueDates


tests :: Tests ()
tests = do
  group "incomeSince" testIncomeSince
  group "incomeUntil" testIncomeUntil
  group "dueDates" testDueDates
  group "neededForBill" testNeededForBill


testIncomeSince :: Tests ()
testIncomeSince = do
  let amt = Money.fromFloat 100.00
  test "one income" $ do
    Needed.incomeSince (parseDay "2019-02-01") (paychecks amt [parseDay "2019-02-15"]) @?= Money.fromFloat 100.00

  test "3x income, including beg date " $ do
    Needed.incomeSince (parseDay "2019-02-01") (paychecks amt [parseDay "2019-02-15", parseDay "2019-02-01", parseDay "2019-02-28"]) @?= Money.fromFloat 300.00

  test "exclude income before" $ do
    Needed.incomeSince (parseDay "2019-02-01") (paychecks amt [parseDay "2019-01-01", parseDay "2019-01-15", parseDay "2019-02-01", parseDay "2019-02-15"]) @?= Money.fromFloat 200.00


testIncomeUntil :: Tests ()
testIncomeUntil = do
  test "weekly" $ do
    let today = parseDay "2019-02-01" -- friday
    let due   = parseDay "2019-03-01" -- friday
    let income = Budget "" (Weekly Monday) (Money.fromFloat 100.00)
    Needed.incomeUntil today due income @?= Money.fromFloat 400.00

  test "monthly, on same date, do not include" $ do
    let today = parseDay "2019-02-01" -- friday
    let due   = parseDay "2019-03-01" -- friday
    let income = Budget "" (Monthly (DayOfMonth 1)) (Money.fromFloat 100.00)
    Needed.incomeUntil today due income @?= Money.fromFloat 0

  test "monthly, on later" $ do
    let today = parseDay "2019-02-01" -- friday
    let due   = parseDay "2019-03-05" -- friday
    let income = Budget "" (Monthly (DayOfMonth 1)) (Money.fromFloat 100.00)
    Needed.incomeUntil today due income @?= Money.fromFloat 100.0



testDueDates :: Tests ()
testDueDates = do
  group "monthly rent, monthly paychecks" $ do
    let payAmount = Money.fromFloat 2000.00
    let income = Budget "" (Monthly (DayOfMonth 1)) (payAmount)
    let bill = Budget "" (Monthly (DayOfMonth 5)) (Money.fromFloat 1000.00)
    test "just paid, due soon" $ do
      dueDates (parseDay "2019-03-02") income bill @?= [parseDay "2019-03-05"]
    test "payday, due soon" $ do
      dueDates (parseDay "2019-03-01") income bill @?= [parseDay "2019-03-05"]
    test "just before payday" $ do
      dueDates (parseDay "2019-02-28") income bill @?= [parseDay "2019-03-05"]


  group "weekly bill, monthly paycheck" $ do
    let payAmount = Money.fromFloat 2000.00
    let schedule = Monthly (DayOfMonth 1)
    let income = Budget "" schedule (payAmount)
    let bill = Budget "" (Weekly Monday) (Money.fromFloat 100.00)
    test "just paid" $ do
      dueDates (parseDay "2019-03-02") income bill @?= [parseDay "2019-03-04", parseDay "2019-03-11", parseDay "2019-03-18", parseDay "2019-03-25", parseDay "2019-04-01"]

    test "week later" $ do
      dueDates (parseDay "2019-03-09") income bill @?= [parseDay "2019-03-11", parseDay "2019-03-18", parseDay "2019-03-25", parseDay "2019-04-01"]




testNeededForBill :: Tests ()
testNeededForBill = do
  group "monthly rent, monthly paychecks" $ do
    let payAmount = Money.fromFloat 2000.00
    let schedule = Monthly (DayOfMonth 1)
    let income = Budget "" schedule (payAmount)
    let bill = Budget "" (Monthly (DayOfMonth 5)) (Money.fromFloat 1000.00)

    test "just paid, due soon" $ do
      let today = parseDay "2019-03-02"
      let checks = [parseDay "2019-02-01", parseDay "2019-03-01"]
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 1000)

    test "payday, due soon" $ do
      let today = parseDay "2019-03-01"
      let checks = [parseDay "2019-02-01", parseDay "2019-03-01"]
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 1000)

    test "just before payday" $ do
      let today = parseDay "2019-02-28"
      let checks = [parseDay "2019-02-01"]
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 0)

    -- but we can't get it there in time...
    -- so maybe it would be better if we didn't do it like this
    test "day of bill" $ do
      let today = parseDay "2019-03-05"
      let checks = [parseDay "2019-02-01", parseDay "2019-03-01"]
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 1000)

    test "paycheck is late" $ do
      let today = parseDay "2019-03-04"
      let checks = [parseDay "2019-02-01", parseDay "2019-03-05"]
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 1000)

    test "paychecks don't match, paid 1x $500, due tomorrow" $ do
      let today = parseDay "2019-03-04"
      -- they were paid 500, not 2000, and there are no more paychecks before the next due date
      neededForBill today (paychecks (Money.fromFloat 500) [parseDay "2019-02-20"]) income bill @?= (Money.fromFloat 1000)

    test "paychecks don't match, paid 2x $500, due later" $ do
      let today = parseDay "2019-02-16"
      let checks = [parseDay "2019-02-01", parseDay "2019-02-15"]
      neededForBill today (paychecks (Money.fromFloat 500) checks) income bill @?= (Money.fromFloat 200)





  group "monthly rent, weekly paychecks" $ do
    let year = parseDay "2019-01-01"
    let payAmount = Money.fromFloat 500.00
    let schedule = (Weekly Sunday)
    let income = Budget "" schedule (payAmount)
    let bill = Budget "" (Monthly (DayOfMonth 5)) (Money.fromFloat 1000.00)

    test "one more paycheck" $ do
      let today = parseDay "2019-03-01" -- Friday
      let checks = Schedule.until (< today) schedule year
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 750.00)

    test "no more paychecks" $ do
      let today = parseDay "2019-03-04" -- Monday
      let checks = Schedule.until (< today) schedule year
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 1000.00)

    test "mid month" $ do
      let today = parseDay "2019-02-15"
      let checks = Schedule.until (< today) schedule year
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 250.00)

    test "just paid it" $ do
      let today = parseDay "2019-02-06"
      let checks = Schedule.until (< today) schedule year
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 0)

    test "due today" $ do
      -- be conservative! We need the money today
      let today = parseDay "2019-03-05"
      let checks = Schedule.until (< today) schedule year
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 1000)


  group "weekly bill, monthly paycheck" $ do
    let payAmount = Money.fromFloat 2000.00
    let schedule = Monthly (DayOfMonth 1)
    let income = Budget "" schedule payAmount
    let bill = Budget "" (Weekly Monday) (Money.fromFloat 100.00)
    test "just paid" $ do
      let today = parseDay "2019-03-02"
      let checks = [parseDay "2019-02-01", parseDay "2019-03-01"]
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 500.00)

    test "week later" $ do
      let today = parseDay "2019-03-09"
      let checks = [parseDay "2019-02-01", parseDay "2019-03-01"]
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 400.00)




paychecks :: Money -> [Day] -> [Transaction a]
paychecks amt ds = map (Transaction "" amt) ds
