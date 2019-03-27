{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate.AccountHealth where

import           Data.Model.Money                   (Money)
import qualified Data.Model.Money                   as Money
import           Data.Time.Calendar                 (Day)
import           Test.Dates                         (parseDay)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Evaluate.AccountHealth      as Health
import           Timely.Evaluate.Health.Abs         (absolute)
import           Timely.Evaluate.Health.Transaction (Transaction (Transaction))
import           Timely.Evaluate.Schedule           (DayOfMonth (..), DayOfWeek (..), Schedule (..))
import qualified Timely.Evaluate.Schedule           as Schedule


specHealth = defaultMain $ testGroup "tests" $ runTests tests

tests :: Tests ()
tests = do
  group "incomeSince" testIncomeSince
  group "incomeUntil" testIncomeUntil
  group "neededForBill" testNeededForBill


testIncomeSince :: Tests ()
testIncomeSince = do
  let amt = Money.fromFloat 100.00
  test "one income" $ do
    Health.incomeSince (parseDay "2019-02-01") (paychecks amt [parseDay "2019-02-15"]) @?= Money.fromFloat 100.00

  test "3x income, including beg date " $ do
    Health.incomeSince (parseDay "2019-02-01") (paychecks amt [parseDay "2019-02-15", parseDay "2019-02-01", parseDay "2019-02-28"]) @?= Money.fromFloat 300.00

  test "exclude income before" $ do
    Health.incomeSince (parseDay "2019-02-01") (paychecks amt [parseDay "2019-01-01", parseDay "2019-01-15", parseDay "2019-02-01", parseDay "2019-02-15"]) @?= Money.fromFloat 200.00


testIncomeUntil :: Tests ()
testIncomeUntil = do
  test "weekly" $ do
    let today = parseDay "2019-02-01" -- friday
    let due   = parseDay "2019-03-01" -- friday
    let income = Scheduled (Weekly Monday) (absolute $ Money.fromFloat 100.00)
    Health.incomeUntil today due income @?= Money.fromFloat 400.00

  test "monthly, on same date, do not include" $ do
    let today = parseDay "2019-02-01" -- friday
    let due   = parseDay "2019-03-01" -- friday
    let income = Scheduled (Monthly (DayOfMonth 1)) (absolute $ Money.fromFloat 100.00)
    Health.incomeUntil today due income @?= Money.fromFloat 0

  test "monthly, on later" $ do
    let today = parseDay "2019-02-01" -- friday
    let due   = parseDay "2019-03-05" -- friday
    let income = Scheduled (Monthly (DayOfMonth 1)) (absolute $ Money.fromFloat 100.00)
    Health.incomeUntil today due income @?= Money.fromFloat 100.0




testNeededForBill :: Tests ()
testNeededForBill = do

  group "monthly rent, monthly paychecks" $ do
    let payAmount = Money.fromFloat 2000.00
    let schedule = Monthly (DayOfMonth 1)
    let income = Scheduled schedule (absolute payAmount)
    let bill = Scheduled (Monthly (DayOfMonth 5)) (absolute $ Money.fromFloat 1000.00)

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

    test "day of bill" $ do
      let today = parseDay "2019-03-05"
      let checks = [parseDay "2019-02-01", parseDay "2019-03-01"]
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 1000)

    test "paycheck is late" $ do
      let today = parseDay "2019-03-04"
      let checks = [parseDay "2019-02-01", parseDay "2019-03-05"]
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 1000)




  group "monthly rent, weekly paychecks" $ do
    let year = parseDay "2019-01-01"
    let payAmount = Money.fromFloat 500.00
    let schedule = (Weekly Sunday)
    let income = Scheduled schedule (absolute payAmount)
    let bill = Scheduled (Monthly (DayOfMonth 5)) (absolute $ Money.fromFloat 1000.00)

    test "one more paycheck" $ do
      let today = parseDay "2019-03-01" -- Friday
      let checks = Schedule.until today schedule year
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 750.00)

    test "no more paychecks" $ do
      let today = parseDay "2019-03-04" -- Monday
      let checks = Schedule.until today schedule year
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 1000.00)

    test "mid month" $ do
      let today = parseDay "2019-02-15"
      let checks = Schedule.until today schedule year
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 250.00)

    test "just paid it" $ do
      let today = parseDay "2019-02-06"
      let checks = Schedule.until today schedule year
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 0)

    test "due today" $ do
      -- be conservative! We need the money today
      let today = parseDay "2019-03-05"
      let checks = Schedule.until today schedule year
      neededForBill today (paychecks payAmount checks) income bill @?= (Money.fromFloat 1000)



paychecks :: Money -> [Day] -> [Transaction a]
paychecks amt ds = map (Transaction (absolute amt)) ds
