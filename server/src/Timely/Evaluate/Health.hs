{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Evaluate.Health
  ( Budget(Budget)
  , Transaction(Transaction)
  , Expense, Income
  , AccountHealth(..)
  , Bill(..)
  , neededForBill
  , dueDates
  , incomeSince
  , incomeUntil

  ) where

import           Data.Aeson                         (ToJSON)
import           Data.Model.Money                   (Money)
import qualified Data.Model.Money                   as Money
import           Data.Number.Abs                    (Abs (..))
import           Data.Time.Calendar                 (Day)
import           GHC.Generics                       (Generic)
import           Timely.Evaluate.Health.Budget      (Budget (..))
import           Timely.Evaluate.Health.Transaction (Expense, Income, Transaction)
import qualified Timely.Evaluate.Health.Transaction as Trans
import qualified Timely.Evaluate.Schedule           as Schedule



-- this is the information reported to the user
data AccountHealth = AccountHealth
  { balance  :: Money
  , budgeted :: Abs Money
  , income   :: Budget Income
  , bills    :: [Bill]
  } deriving (Show, Eq, Generic)

instance ToJSON AccountHealth


data Bill = Bill
  { saved  :: Abs Money
  , next   :: Day
  , budget :: Budget Expense
  } deriving (Show, Eq, Generic)

instance ToJSON Bill







-- do we include the start date? Yes, be conservative
incomeSince :: Day -> [Transaction Income] -> Money
incomeSince start =
  sum . map (value . Trans.amount) . filter (\t -> Trans.date t >= start)


-- where Day is the bill due date
incomeUntil :: Day -> Day -> Budget Income -> Money
incomeUntil today due Budget {schedule, amount} =
  Money.fromCents $ length (Schedule.until (< due) schedule today) * Money.toCents (value amount)



-- neededForBills :: Day -> [Transaction Income] -> Budget Income -> [Budget Expense] -> Money
-- neededForBills today paychecks income bills =
--   sum $ map (neededForBill today paychecks income) bills


-- this is just for the NEXT instance of the bill
-- we just need to duplicate this for each time the bill comes due before the next paycheck. Isn't this kind of circular

neededForBill :: Day -> [Transaction Income] -> Budget Income -> Budget Expense -> Money
neededForBill today paychecks income bill =
  let lastDue = Schedule.last (schedule bill) today
      dates   = dueDates today income bill
  in sum $ map (neededForNextBill paychecks income (amount bill) lastDue) dates



dueDates :: Day -> Budget Income -> Budget Expense -> [Day]
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
neededForNextBill :: [Transaction Income] -> Budget Income -> Abs Money -> Day -> Day -> Money
neededForNextBill paychecks income amount lastDue nextDue =
  let incPrev = incomeSince lastDue paychecks
      incTotal = incomeUntil lastDue nextDue income
      percent = if incTotal > 0
                   then (Money.toFloat incPrev / Money.toFloat incTotal)
                   else 1.0
  in Money.fromFloat $ percent * (Money.toFloat $ value amount)