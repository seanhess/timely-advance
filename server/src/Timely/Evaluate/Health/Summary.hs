{-# LANGUAGE DuplicateRecordFields #-}

module Timely.Evaluate.Health.Summary where

import Data.Model.Money                   as Money (Money)
import Data.Time.Calendar                 (Day)
import Timely.Evaluate.Health.Budget      as Budget (Budget)
import Timely.Evaluate.Health.Transaction (Expense)


-- Balance
----------------
-- - Bills
-- - Spending (summed up!)
-- + Timely Advance
-- = Minimum
----------------
-- Paycheck
-- -Advance


-- | The spending for a day. We don't include advances, because the advance system is responsible for sending the money. It's just a guarantee we'll send one in time
data Debits = Debits
  { spending :: Money
  , bills    :: [Budget Expense]
  }

-- | The Daily spending applied to a date and the balance
data DailyBalance = DailyBalance
  { date    :: Day
  , debits  :: Debits
  , balance :: Money
  }

data Scheduled a = Scheduled
  { date   :: Day
  , budget :: Budget a
  }

-- you can use Scheduled like Event
-- modify Timeline

-- Ok, this is the wrong name


-- | Balance is not in here, since it's an input to this whole thing, right? But then, so is dailySpending
-- dailySpending
-- initial balance
-- this isn't the object you send back. It's my report.
-- remember the parent can calculate totals
-- this is in the parent!
-- all we retun is [DailyBalance]
-- data Summary = Summary
--   { totalBills     :: Abs Money
--   , totalSpending  :: Abs Money
--   , minimumBalance :: Money
--   , advance        :: Maybe (Abs Money)
--   , timeline       :: [ DailyBalance ]
--   , paycheck       :: Scheduled Income
--   }


-- Parent: AccountHealth, has this summary object
