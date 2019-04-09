{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Api.Transactions where


import Control.Effects (MonadEffects)
import           Data.Aeson                         (ToJSON)
import           Data.Model.Guid                    (Guid)
import Data.Maybe (mapMaybe)
import           Data.Number.Abs                    (absolute)
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
  (History.groups $ mapMaybe toIncome ts)
  (History.groups $ mapMaybe toExpense ts)


recent :: MonadEffects '[Accounts] m => Guid Account -> m [TransactionRow]
recent i =
  Accounts.listTransactions i 0 100


history :: MonadEffects '[Accounts] m => Guid Account -> m History
history i = do
  ts <- recent i
  pure $ rowsToHistory ts




toIncome :: TransactionRow -> Maybe (Transaction Income)
toIncome row@TransactionRow {name, date, amount} =
  if isIncome row
    then Just $ Transaction name (absolute amount) date
    else Nothing


toExpense :: TransactionRow -> Maybe (Transaction Expense)
toExpense row@TransactionRow {name, date, amount} =
  if isExpense row
    then Just $ Transaction name (absolute amount) date
    else Nothing
