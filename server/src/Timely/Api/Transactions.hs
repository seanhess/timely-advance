{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Api.Transactions where


import Control.Effects (MonadEffects)
import           Data.Aeson                         (ToJSON)
import qualified Data.List                          as List
import           Data.Model.Guid                    (Guid)
import           Data.Number.Abs                    (Abs (value), absolute)
import           GHC.Generics                       (Generic)
import           Timely.Accounts                    (Account, Accounts, TransactionRow(..))
import qualified Timely.Accounts                    as Accounts
import           Timely.Evaluate.Health.Transaction (Expense, Income, Transaction (..))
import           Timely.Evaluate.History            (Group)
import qualified Timely.Evaluate.History            as History



data History = History
  { income   :: [Group Income]
  , expenses :: [Group Expense]
  } deriving (Show, Eq, Generic)

instance ToJSON History



isExpense :: TransactionRow -> Bool
isExpense TransactionRow {amount} =
  amount >= 0


isIncome :: TransactionRow -> Bool
isIncome TransactionRow {amount} =
  amount < 0


rowsToHistory :: [TransactionRow] -> History
rowsToHistory ts = History
  (History.groups $ List.map fromRow $ List.filter isIncome ts)
  (History.groups $ List.map fromRow $ List.filter isExpense ts)


recent :: MonadEffects '[Accounts] m => Guid Account -> m [TransactionRow]
recent i =
  Accounts.listTransactions i 0 100


history :: MonadEffects '[Accounts] m => Guid Account -> m History
history i = do
  ts <- recent i
  pure $ rowsToHistory ts



fromRow :: TransactionRow -> Transaction a
fromRow TransactionRow {name, date, amount} =
  Transaction name (absolute amount) date
