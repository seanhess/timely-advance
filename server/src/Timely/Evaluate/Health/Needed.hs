-- DEPRECATED -- this is the old method
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Evaluate.Health.Needed
  ( Transaction
  , Expense, Income
  , neededForBill
  , dueDates
  , incomeSince
  , incomeUntil

  ) where

-- import Debug.Trace (traceShow)
import           Data.Model.Money                   (Money)
import qualified Data.Model.Money                   as Money
import           Data.Number.Abs                    (Abs (..))
import           Data.Time.Calendar                 (Day)
import           Timely.Evaluate.Health.Budget      (BudgetInfo(..))
import           Timely.Evaluate.Health.Transaction (Expense, Income, Transaction)
import qualified Timely.Evaluate.Health.Transaction as Trans
import qualified Timely.Evaluate.Schedule           as Schedule










-- do we include the start date? Yes, be conservative
incomeSince :: Day -> [Transaction Income] -> Money
incomeSince start =
  sum . map (Trans.amount) . filter (\t -> Trans.date t >= start)


-- where Day is the bill due date
incomeUntil :: Day -> Day -> BudgetInfo Income -> Money
incomeUntil today due BudgetInfo {schedule, amount} =
  Money.fromCents $ length (Schedule.until (< due) schedule today) * Money.toCents (value amount)



-- neededForBills :: Day -> [Transaction Income] -> Budget Income -> [Budget Expense] -> Money
-- neededForBills today paychecks income bills =
--   sum $ map (neededForBill today paychecks income) bills


-- this is just for the NEXT instance of the bill
-- we just need to duplicate this for each time the bill comes due before the next paycheck. Isn't this kind of circular

neededForBill :: Day -> [Transaction Income] -> BudgetInfo Income -> BudgetInfo Expense -> Money
neededForBill today paychecks income bill =
  let lastDue = Schedule.last (schedule bill) today
      dates   = dueDates today income bill
  in sum $ map (neededForNextBill paychecks income (amount bill) today lastDue) dates



dueDates :: Day -> BudgetInfo Income -> BudgetInfo Expense -> [Day]
dueDates today income bill =
  -- if nextDue is before nextPay, then keep giving due dates until the next pay

  -- if the bill is due today, we want nextDue to be today
  -- but next normally skips the current day, so start yesterday
  -- to be conservative, assume the bill is due today, but we are not being paid today
  let nextPay = Schedule.next      (schedule income) today
      nextDue = Schedule.nextToday (schedule bill)   today
  in if nextDue < nextPay
    then Schedule.untilToday (<= nextPay) (schedule bill) today
    else [nextDue]



-- oh, there is no income in this period
neededForNextBill :: [Transaction Income] -> BudgetInfo Income -> Abs Money -> Day -> Day -> Day -> Money
neededForNextBill paychecks income amount today lastDue nextDue =
  let incPrev = incomeSince lastDue paychecks
      incNext = incomeUntil today   nextDue income
      incTotal = incPrev + incNext
      percent = if incTotal > 0
                   then (Money.toFloat incPrev / Money.toFloat incTotal)
                   else 1.0
  in Money.fromFloat $ percent * (Money.toFloat $ value amount)
