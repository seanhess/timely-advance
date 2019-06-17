{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Actions.AccountHealth where

import           Control.Effects                   (MonadEffect, MonadEffects)
import           Control.Effects.Signal            (Throw, throwSignal)
import           Control.Effects.Time              (Time)
import qualified Control.Effects.Time              as Time
import           Data.Aeson                        (ToJSON)
import           Data.Function                     ((&))
import qualified Data.List                         as List
import           Data.Maybe                        (listToMaybe, fromMaybe)
import           Data.Model.Guid                   (Guid)
import qualified Data.Model.Meta                   as Meta
import           Data.Model.Money                  as Money (Money, fromFloat)
import           Data.Number.Abs                   (Abs (value), absolute)
import           Data.Time.Calendar                (Day)
import           GHC.Generics                      (Generic)
import           Timely.Accounts                   (Accounts)
import qualified Timely.Accounts                   as Accounts
import           Timely.Accounts.Budgets           (BudgetMeta, Budgets)
import qualified Timely.Accounts.Budgets           as Budgets
import           Timely.Accounts.Types             (Account, BankAccount (..), TransactionRow)
import qualified Timely.Accounts.Types.BankAccount as BankAccount
import qualified Timely.Actions.Transactions       as Transactions
import           Timely.Advances                   as Advances (Advance(..), Advances, findActive)
import           Timely.Evaluate.Health            as Health (DailyBalance, Expense, Income)
import           Timely.Evaluate.Health.Budget     as Budget (Budget (..), Scheduled (..))
import           Timely.Evaluate.Health.Timeline   as Health
import           Timely.Evaluate.Schedule          as Schedule (next)
import           Timely.Types.Update               (Error (..))









data AccountHealth = AccountHealth
  { balance       :: Money
  , minimum       :: Money
  , spendingDaily :: Abs Money
  , spendingTotal :: Abs Money
  , billsTotal    :: Abs Money
  , dailyBalances :: [DailyBalance]
  , advance       :: Maybe Advance
  , paycheck      :: Scheduled Income
  , bills         :: [Scheduled Expense]
  } deriving (Show, Eq, Generic)

instance ToJSON AccountHealth





analyze :: (MonadEffects '[Budgets, Accounts, Advances, Throw Error, Time] m) => Guid Account -> m AccountHealth
analyze i = do
    now   <- Time.currentDate
    pays  <- Budgets.getIncomes i
    pay   <- primaryIncome i pays
    bills <- Budgets.getExpenses i
    trans <- Transactions.recent i
    advs  <- Advances.findActive i
    check <- loadChecking i
    pure $ analyzeWith now check pay bills trans advs

  where
    loadChecking i = do
      banks <- Accounts.findBanks i
      List.find BankAccount.isChecking banks
          & required (NoChecking i)



analyzeWith :: Day -> BankAccount -> Budget Income -> [BudgetMeta Expense] -> [TransactionRow] -> [Advance] -> AccountHealth
analyzeWith now BankAccount {balance} pay bms _ advs =

    let payday   = Schedule.next (schedule pay) now
        paycheck = Scheduled pay payday
        bs    = List.map Meta.value bms

        -- TODO calculate this from their transactions, store it somewhere!
        spendingDaily = absolute $ Money.fromFloat 30.00

        dailys = Health.timeline now payday spendingDaily bs
        dailyBalances = Health.dailyBalances balance dailys


        advance = listToMaybe advs
        advanceAmount = fmap Advances.amount advance

        -- TODO how do we handle advances in the calculation? Let's assume we haven't sent it yet. It's promised, but not sent. But the minute we sent it we need to mark it as sent and calculate it differently, because their accoutn isn't in jeopardy yet. Or we need some way to tell if it's actually hit (Using their transactions!)
        minimum = Health.minimumBalance balance dailyBalances + (fromMaybe (Money.fromFloat 0) advanceAmount)

        bills = Health.billsDue dailys
        billsTotal = absolute $ List.sum $ List.map (value . Budget.amount . budget) bills
        spendingTotal = Health.totalSpending dailys


    -- Not using transactions for now, simplify because we can't actually
    -- take any action if things are settling today. We can only move
    -- one day out
    in AccountHealth
        { balance
        , minimum
        , spendingDaily
        , spendingTotal
        , dailyBalances
        , advance
        , paycheck
        , bills
        , billsTotal
        }



primaryIncome
  :: ( MonadEffects '[Throw Error] m )
  => Guid Account -> [BudgetMeta Income] -> m (Budget Income)
primaryIncome accountId pays =
  pays
    & List.map Meta.value
    & List.sortOn (value . Budget.amount)
    & listToMaybe
    & required (NoIncome accountId)



required :: MonadEffect (Throw Error) m => Error -> Maybe a -> m a
required = required' id


required' :: MonadEffect (Throw Error) m => (a -> Maybe b) -> Error -> a -> m b
required' f e ma =
  case f ma of
    Nothing -> throwSignal e
    Just b  -> pure b
