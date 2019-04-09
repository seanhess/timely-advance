{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Api.AccountHealth where

import           Control.Effects                    (MonadEffect, MonadEffects)
import           Control.Effects.Signal             (Throw, throwSignal)
import           Control.Effects.Time               (Time)
import qualified Control.Effects.Time               as Time
import           Data.Function                      ((&))
import qualified Data.List                          as List
import           Data.Maybe                         (mapMaybe)
import           Data.Model.Guid                    (Guid)
import           Data.Model.Money                   (Money)
import           Data.Number.Abs                    (Abs (value), absolute)
import           Data.Time.Calendar                 (Day)
import           GHC.Generics                       (Generic)
import           Timely.Accounts                    (Accounts)
import qualified Timely.Accounts                    as Accounts
import           Timely.Accounts.Budgets            as Budgets
import           Timely.Accounts.Types              (Account)
import qualified Timely.Accounts.Types.BankAccount  as BankAccount
import           Timely.Api.Transactions            (toIncome)
import qualified Timely.Api.Transactions            as Transactions
import           Timely.Evaluate.Health             (AccountHealth (..), Bill (..), Budget, Expense, Income)
import qualified Timely.Evaluate.Health             as Health
import           Timely.Evaluate.Health.Budget      (Budget (..))
import           Timely.Evaluate.Health.Transaction (Transaction (..))
import qualified Timely.Evaluate.Schedule           as Schedule
import           Timely.Types.Update                (Error(..))










-- TODO function that converts this into something user readable
-- Either Error AccountHealth
-- then convert it into something that can be serialized
-- I need a type that just serializes either one, easy


analyze :: (MonadEffects '[Budgets, Accounts, Throw Error, Time] m) => Guid Account -> m AccountHealth
analyze i = do
    now <- Time.currentDate
    inc <- budgetIncome i
    exs <- budgetExpenses i
    bal <- bankBalance i
    tns <- mapMaybe toIncome <$> Transactions.recent i

    let bills = map (bill now tns inc) exs

    pure $ AccountHealth
      { balance = bal
      , income = inc
      , budgeted = absolute $ sum (map (value . saved) bills)
      , bills = bills
      }

  where
    budgetIncome i   = Budgets.income i   >>= required (MissingIncome i)
    budgetExpenses i = Budgets.expenses i >>= required' Just (MissingExpenses i)
    bankBalance i = do
      banks <- Accounts.findBanks i
      check <- List.find BankAccount.isChecking banks
                & required (MissingChecking i)
      pure $ BankAccount.balance check




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
