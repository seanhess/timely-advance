{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Actions.AccountHealth where

import           Control.Effects                    (MonadEffect, MonadEffects)
import           Control.Effects.Signal             (Throw, throwSignal)
import           Control.Effects.Time               (Time)
import qualified Control.Effects.Time               as Time
import           Data.Function                      ((&))
import qualified Data.List                          as List
-- import           Data.Maybe                         (mapMaybe)
import           Data.Model.Guid                    (Guid)
import           Data.Time.Calendar                 (Day) -- addDays
-- import qualified Data.Time.Calendar                 as Day
import           Timely.Accounts                    (Accounts)
import qualified Timely.Accounts                    as Accounts
import           Timely.Accounts.Budgets            (Budgets)
import qualified Timely.Accounts.Budgets            as Budgets
import           Timely.Accounts.Types              (Account, BankAccount, TransactionRow)
import qualified Timely.Accounts.Types.BankAccount  as BankAccount
-- import           Timely.Actions.Transactions        (toIncome)
import qualified Timely.Actions.Transactions        as Transactions
import           Timely.Evaluate.Health.Budget      (Budget (..))
import           Timely.Evaluate.Health.Transaction (Expense, Income)
-- import           Timely.Evaluate.Health.Transaction (Transaction (..))
-- import qualified Timely.Evaluate.Schedule           as Schedule
import           Timely.Types.AccountHealth         (AccountHealth (..))
import           Timely.Types.Update                (Error (..))







-- TODO function that converts this into something user readable
-- Either Error AccountHealth






analyze :: (MonadEffects '[Budgets, Accounts, Throw Error, Time] m) => Guid Account -> m AccountHealth
analyze i = do
    now <- Time.currentDate
    inc <- Budgets.getIncomes i
    exs <- Budgets.getExpenses i
    tns <- Transactions.recent i
    chk <- loadChecking i
    pure $ analyzeWith now chk inc exs tns

  where
    loadChecking i = do
      banks <- Accounts.findBanks i
      List.find BankAccount.isChecking banks
          & required (NoChecking i)



analyzeWith :: Day -> BankAccount -> [Budget Income] -> [Budget Expense] -> [TransactionRow] -> AccountHealth
analyzeWith now check incs exps trans = do
    let balance = BankAccount.balance check
    undefined now incs exps trans balance
        -- events  =

    -- today: rent is due, has it already been paid?
    -- rent just got paid. I need to know if it is included.

    --     tns = mapMaybe toIncome trans
    --     bills = map (bill now tns incs) exps
    --     checks = filter (isRecent now) tns
    --     budgeted = absolute $ sum (map (saved) bills)

    -- in AccountHealth
    --   { balance = bal
    --   , income = incm
    --   , budgeted = budgeted
    --   , spending = bal - budgeted
    --   , bills = bills
    --   , paychecks = checks
    --   }

  -- where
    -- isRecent now t = date t >= addDays (-30) now



-- all the events for the next calendar month for a given Budget
-- we don't know if they're positive or negative from the type
-- but we should look into the transaction history, to see if they've already landed
-- only if they've landed today?
-- TRANSACTION: just happened. Rent was deducted. It's due today. The balance is 1500 lower. The balance is now 200.

-- events :: Day -> Budget a -> [Transaction a]
-- events now Budget {schedule, name, amount} =
--    let end   = Day.addGregorianMonthsClip 1 now
--        dates = until (<= end) schedule now
--    in List.map (\d -> Event {})


-- bill :: Day -> [Transaction Income] -> Budget Income -> Budget Expense -> Bill
-- bill now paychecks income budget@(Budget {schedule}) =
--   Bill
--     { saved  = absolute $ Health.neededForBill now paychecks income budget
--     , next   = Schedule.nextToday schedule now
--     , budget = budget
--     }





-- scheduledBill :: Budget a -> Scheduled a
-- scheduledBill



required :: MonadEffect (Throw Error) m => Error -> Maybe a -> m a
required = required' id


required' :: MonadEffect (Throw Error) m => (a -> Maybe b) -> Error -> a -> m b
required' f e ma =
  case f ma of
    Nothing -> throwSignal e
    Just b  -> pure b
