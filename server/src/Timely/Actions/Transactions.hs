{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Actions.Transactions where


import           Control.Effects (MonadEffects)
import           Data.Aeson                         (ToJSON)
import           Data.Model.Guid                    (Guid)
import qualified Control.Effects.Time               as Time
import           Control.Effects.Time               (Time)
import qualified Data.List                          as List
import qualified Data.Model.Money                   as Money
import           Data.Maybe                         (mapMaybe, fromMaybe)
import           GHC.Generics                       (Generic)
import           Data.Number.Abs                    (Abs(value))
import           Timely.Accounts                    (Account, Accounts, TransactionRow(..))
import qualified Timely.Accounts                    as Accounts
import           Timely.Evaluate.Health.Transaction (Expense, Income, Transaction (..))
import qualified Timely.Evaluate.Health.Transaction as Trans
import           Timely.Evaluate.Health.Budget (Budget(..))
import           Timely.Evaluate.History            (Group(..))
import           Timely.Evaluate.Schedule (scheduleDate, Schedule(Monthly))
import qualified Timely.Evaluate.History            as History



data History = History
  { income   :: [Group Income]
  , expenses :: [Group Expense]
  } deriving (Show, Eq, Generic)

instance ToJSON History



-- is there any way to make this throw an error?
-- yeah, don't allow them to be comparable / ord
isExpense :: TransactionRow -> Bool
isExpense TransactionRow {amount} =
  amount >= 0


isIncome :: TransactionRow -> Bool
isIncome TransactionRow {amount} =
  amount < 0


history :: [TransactionRow] -> History
history ts = History
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




defaultBudget :: Group a -> Budget a
defaultBudget Group {name, average, schedule, transactions} =
  let date = scheduleDate $ List.map Trans.date transactions
  in Budget
    { name
    , schedule = fromMaybe (Monthly date) schedule
    , amount = average
    }





toIncome :: TransactionRow -> Maybe (Transaction Income)
toIncome row@TransactionRow {name, date, amount} =
  if isIncome row
    then Just $ Trans.income name amount date
    else Nothing


toExpense :: TransactionRow -> Maybe (Transaction Expense)
toExpense row@TransactionRow {name, date, amount} =
  if isExpense row
    then Just $ Trans.expense name amount date
    else Nothing
