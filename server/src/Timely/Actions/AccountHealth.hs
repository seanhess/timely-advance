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
import           Data.Maybe                         (mapMaybe)
import           Data.Model.Guid                    (Guid)
import           Data.Number.Abs                    (Abs (value), absolute)
import           Data.Time.Calendar                 (Day, addDays)
import           Timely.Accounts                    (Accounts)
import qualified Timely.Accounts                    as Accounts
import           Timely.Accounts.Budgets            (Budgets)
import qualified Timely.Accounts.Budgets            as Budgets
import           Timely.Accounts.Types              (Account, BankAccount, TransactionRow)
import qualified Timely.Accounts.Types.BankAccount  as BankAccount
import           Timely.Actions.Transactions        (toIncome)
import qualified Timely.Actions.Transactions        as Transactions
import           Timely.Evaluate.Health             (Budget, Expense, Income)
import qualified Timely.Evaluate.Health             as Health
import           Timely.Evaluate.Health.Budget      (Budget (..))
import           Timely.Evaluate.Health.Transaction (Transaction (..))
import qualified Timely.Evaluate.Schedule           as Schedule
import           Timely.Types.AccountHealth         (AccountHealth (..), Bill (..))
import           Timely.Types.Update                (Error (..))







-- TODO function that converts this into something user readable
-- Either Error AccountHealth






analyze :: (MonadEffects '[Budgets, Accounts, Throw Error, Time] m) => Guid Account -> m AccountHealth
analyze i = do
    now <- Time.currentDate
    inc <- budgetIncome i
    exs <- budgetExpenses i
    tns <- Transactions.recent i
    chk <- loadChecking i
    pure $ analyzeWith now chk inc exs tns

  where
    budgetIncome i   = Budgets.income i   >>= required (NoIncome i)
    budgetExpenses i = Budgets.expenses i
    loadChecking i = do
      banks <- Accounts.findBanks i
      List.find BankAccount.isChecking banks
          & required (NoChecking i)



analyzeWith :: Day -> BankAccount -> Budget Income -> [Budget Expense] -> [TransactionRow] -> AccountHealth
analyzeWith now check incm exps trans =
    let bal = BankAccount.balance check
        tns = mapMaybe toIncome trans
        bills = map (bill now tns incm) exps
        checks = filter (isRecent now) tns
        budgeted = absolute $ sum (map (value . saved) bills)

    in AccountHealth
      { balance = bal
      , income = incm
      , budgeted = budgeted
      , spending = bal - value budgeted
      , bills = bills
      , paychecks = checks
      }

  where
    isRecent now t = date t >= addDays (-30) now


bill :: Day -> [Transaction Income] -> Budget Income -> Budget Expense -> Bill
bill now paychecks income budget@(Budget {schedule}) =
  Bill
    { saved  = absolute $ Health.neededForBill now paychecks income budget
    , next   = Schedule.nextToday schedule now
    , budget = budget
    }





-- scheduledBill :: Budget a -> Scheduled a
-- scheduledBill



required :: MonadEffect (Throw Error) m => Error -> Maybe a -> m a
required = required' id


required' :: MonadEffect (Throw Error) m => (a -> Maybe b) -> Error -> a -> m b
required' f e ma =
  case f ma of
    Nothing -> throwSignal e
    Just b  -> pure b
