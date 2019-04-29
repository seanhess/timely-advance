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
import           Data.Aeson                         (ToJSON)
import           Data.Function                      ((&))
import qualified Data.List                          as List
import           Data.Model.Guid                    (Guid)
import qualified Data.Model.Meta                    as Meta
import           Data.Time.Calendar                 (Day)
import           GHC.Generics                       (Generic)
import           Timely.Accounts                    (Accounts)
import qualified Timely.Accounts                    as Accounts
import           Timely.Accounts.Budgets            (Budgets, BudgetMeta)
import qualified Timely.Accounts.Budgets            as Budgets
import           Timely.Accounts.Types              (Account, BankAccount (..), TransactionRow)
import qualified Timely.Accounts.Types.BankAccount  as BankAccount
import qualified Timely.Actions.Transactions        as Transactions
import           Timely.Evaluate.Health             (Projection)
import qualified Timely.Evaluate.Health             as Health
import           Timely.Evaluate.Health.Transaction (Expense, Income)
import           Timely.Types.Update                (Error (..))









data AccountHealth = AccountHealth
  { projection :: Projection
  } deriving (Show, Eq, Generic)

instance ToJSON AccountHealth





analyze :: (MonadEffects '[Budgets, Accounts, Throw Error, Time] m) => Guid Account -> m AccountHealth
analyze i = do
    now   <- Time.currentDate
    pays  <- Budgets.getIncomes i
    bills <- Budgets.getExpenses i
    trans <- Transactions.recent i
    check <- loadChecking i
    pure $ analyzeWith now check pays bills trans

  where
    loadChecking i = do
      banks <- Accounts.findBanks i
      List.find BankAccount.isChecking banks
          & required (NoChecking i)



analyzeWith :: Day -> BankAccount -> [BudgetMeta Income] -> [BudgetMeta Expense] -> [TransactionRow] -> AccountHealth
analyzeWith now BankAccount {balance} pays bills _ = do
    -- Not using transactions for now, simplify because we can't actually
    -- take any action if things are settling today. We can only move
    -- one day out
    AccountHealth $
       Health.projection now balance
         (map Meta.value pays)
         (map Meta.value bills)





required :: MonadEffect (Throw Error) m => Error -> Maybe a -> m a
required = required' id


required' :: MonadEffect (Throw Error) m => (a -> Maybe b) -> Error -> a -> m b
required' f e ma =
  case f ma of
    Nothing -> throwSignal e
    Just b  -> pure b
