{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Api.Transactions where


import           Control.Effects (MonadEffects)
import           Data.Aeson                         (ToJSON)
import           Data.Model.Guid                    (Guid)
import qualified Control.Effects.Time as Time
import           Control.Effects.Time (Time)
import qualified Data.List as List
import qualified Data.Model.Money as Money
import           Data.Maybe (mapMaybe)
import           Data.Number.Abs                    (absolute, value)
import           GHC.Generics                       (Generic)
import           Timely.Accounts                    (Account, Accounts, TransactionRow(..))
import qualified Timely.Accounts                    as Accounts
import           Timely.Evaluate.Health.Transaction (Expense, Income, Transaction (..))
import           Timely.Evaluate.History            (Group(..))
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
  (List.filter isValidIncome $ History.groups $ mapMaybe toIncome ts)
  (List.filter isValidBill $ History.groups $ mapMaybe toExpense ts)


isValidIncome :: Group Income -> Bool
isValidIncome Group {average, transactions} =
  (value average >= Money.fromFloat 200) &&
  (length transactions >= 2)


isValidBill :: Group Expense -> Bool
isValidBill Group {average, transactions} =
  (value average >= Money.fromFloat 10) &&
  (length transactions >= 2)



-- the last 90 days of transactions
recent :: MonadEffects '[Accounts, Time] m => Guid Account -> m [TransactionRow]
recent i = do
  today <- Time.currentDate
  Accounts.transDays i 90 today


history :: MonadEffects '[Accounts, Time] m => Guid Account -> m History
history i = do
  ts <- recent i
  -- it should only show income over $200
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
