{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Evaluate.AccountHealth where

import Debug.Trace (traceShow)
import           Data.Model.Money                   (Money)
import qualified Data.Model.Money                   as Money
import           Data.Time.Calendar                 (Day, addDays)
import           Timely.Evaluate.Health.Abs         (Abs(..))
import           Timely.Evaluate.Health.Transaction (Transaction)
import qualified Timely.Evaluate.Health.Transaction as Trans
import           Timely.Evaluate.Schedule           (Schedule)
import qualified Timely.Evaluate.Schedule           as Schedule
import           Timely.Evaluate.Types              (Projection (..))



-- TODO more information on "expenses"? Like which expenses do we predict will happen again, etc
-- TODO accurate projected spending

analyze :: Money -> Projection
analyze checking =
    Projection
      { expenses  = projectedSpending
      , available = checking
      }

-- TODO inputs
projectedSpending :: Money
projectedSpending = Money.fromFloat 200.00







data Scheduled a = Scheduled
  { schedule :: Schedule
  , amount   :: Abs Money
  }

data Bill
data Income



-- do we include the start date? Yes, be conservative
incomeSince :: Day -> [Transaction Income] -> Money
incomeSince start =
  sum . map (value . Trans.amount) . filter (\t -> Trans.date t >= start)

-- where Day is the bill due date
incomeUntil :: Day -> Day -> Scheduled Income -> Money
incomeUntil today due Scheduled {schedule, amount} =
  Money.fromCents $ length (Schedule.until due schedule today) * Money.toCents (value amount)



-- One test
neededForBills :: Day -> [Transaction Income] -> Scheduled Income -> [Scheduled Bill] -> Money
neededForBills today paychecks income bills =
  sum $ map (neededForBill today paychecks income) bills


-- this is really simple. Only needs one test
neededForBill :: Day -> [Transaction Income] -> Scheduled Income -> Scheduled Bill -> Money
neededForBill today paychecks income Scheduled {amount, schedule} =
  let lastDue = Schedule.last schedule today
      -- if the bill is due today, we want nextDue to be today
      -- but next normally skips the current day, so start yesterday
      nextDue = Schedule.next schedule (addDays (-1) today)
      incPrev = incomeSince lastDue paychecks
      incTotal = incomeUntil lastDue nextDue income
      percent = (Money.toFloat incPrev / Money.toFloat incTotal)
  in traceShow (incPrev, incTotal) $ Money.fromFloat $ percent * (Money.toFloat $ value amount)




