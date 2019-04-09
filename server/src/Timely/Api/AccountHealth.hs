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
import           Data.Aeson                         (Options (..), SumEncoding (..), ToJSON (..), defaultOptions,
                                                     genericToJSON)
import           Data.Function                      ((&))
import qualified Data.List                          as List
import           Data.Model.Guid                    (Guid)
import           Data.Model.Money                   (Money)
import           Data.Number.Abs                    (Abs (value), absolute)
import           Data.Text                          (Text)
import           Data.Time.Calendar                 (Day)
import           GHC.Generics                       (Generic)
import           Timely.Accounts                    (Accounts)
import qualified Timely.Accounts                    as Accounts
import           Timely.Accounts.Budgets            as Budgets
import           Timely.Accounts.Types              (Account)
import qualified Timely.Accounts.Types              as Accounts
import qualified Timely.Accounts.Types.BankAccount  as BankAccount
import           Timely.Evaluate.Health             (Budget, Expense, Income)
import qualified Timely.Evaluate.Health             as Health
import           Timely.Evaluate.Health.Budget      (Budget (..))
import           Timely.Evaluate.Health.Transaction (Transaction (..))
import           Timely.Evaluate.Schedule           (Schedule)
import qualified Timely.Evaluate.Schedule           as Schedule



-- this is the information reported to the user
data AccountHealth = AccountHealth
  { balance  :: Money
  , budgeted :: Abs Money
  , income   :: Budget Income
  , bills    :: [Bill]
  } deriving (Show, Eq, Generic)

instance ToJSON AccountHealth


data Bill = Bill
  { saved  :: Abs Money
  , next   :: Day
  , budget :: Budget Expense
  } deriving (Show, Eq, Generic)

instance ToJSON Bill




data Error
  = MissingIncome   (Guid Account)
  | MissingExpenses (Guid Account)
  | MissingChecking (Guid Account)
  deriving (Show, Eq, Generic)

instance ToJSON Error where
  toJSON = genericToJSON defaultOptions { sumEncoding = TaggedObject "error" "info" }




-- TODO function that converts this into something user readable
-- Either Error AccountHealth
-- then convert it into something that can be serialized
-- I need a type that just serializes either one, easy


accountHealth :: (MonadEffects '[Budgets, Accounts, Throw Error, Time] m) => Guid Account -> m AccountHealth
accountHealth i = do
    now <- Time.currentDate
    inc <- budgetIncome i
    exs <- budgetExpenses i
    bal <- bankBalance i

    let bills = map (bill now) exs

    -- pure _
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



bill :: Day -> Budget Expense -> Bill
bill now budget@(Budget {schedule}) =
  Bill
    { saved  = absolute $ Health.neededForBill now undefined undefined budget
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
