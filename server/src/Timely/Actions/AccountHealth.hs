{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Timely.Actions.AccountHealth where

import           Control.Effects                   (MonadEffect, MonadEffects)
import           Control.Effects.Signal            (Throw, throwSignal)
import           Control.Effects.Time              (Time)
import qualified Control.Effects.Time              as Time
import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.Function                     ((&))
import qualified Data.List                         as List
import           Data.Maybe                        (fromMaybe, listToMaybe)
import           Data.Model.Guid                   (Guid)
import           Data.Model.Money                  as Money (Money, fromFloat)
import           Data.Number.Abs                   (Abs (value), absolute)
import           Data.Time.Calendar                (Day)
import           GHC.Generics                      (Generic)
import           Timely.Accounts                   (Accounts)
import qualified Timely.Accounts                   as Accounts
import           Timely.Accounts.Budgets           (Budgets)
import qualified Timely.Accounts.Budgets           as Budgets
import           Timely.Accounts.Types             (Account, BankAccount (..), TransactionRow)
import qualified Timely.Accounts.Types.BankAccount as BankAccount
import qualified Timely.Actions.Transactions       as Transactions
import           Timely.Advances                   as Advances (Advance (..), Advances, findActive)
import           Timely.Evaluate.Health            as Health (DailyBalance, Expense, Income)
import           Timely.Evaluate.Health.Budget     as Budget (Budget, BudgetInfo (..), budget)
import           Timely.Evaluate.Health.Daily      as Daily (balance, daily, date)
import           Timely.Evaluate.Health.Scheduled  as Scheduled (Scheduled (..))
import           Timely.Evaluate.Health.Timeline   as Health
import           Timely.Evaluate.Schedule          as Schedule (next)
import           Timely.Types.Update               (Error (..))


newtype Amount a = Amount Money
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Balance
data Minimum
data Last
data After



data AccountHealth = AccountHealth
  { balance       :: Amount Balance
  , minimum       :: Amount Minimum
  , last          :: Amount Last
  , overdraft     :: Maybe Day
  , spendingDaily :: Abs Money
  , spendingTotal :: Abs Money
  , afterPaycheck :: Amount After
  , billsTotal    :: Abs Money
  , dailyBalances :: [DailyBalance]
  , advance       :: Maybe Advance
  , paycheck      :: Scheduled (Budget Income)
  , bills         :: [Scheduled (Budget Expense)]
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
    spend <- Budgets.spending i >>= required (NoSpending i)
    check <- loadChecking i
    pure $ analyzeWith now check pay bills spend trans advs

  where
    loadChecking i = do
      banks <- Accounts.findBanks i
      List.find BankAccount.isChecking banks
          & required (NoChecking i)



analyzeWith :: Day -> BankAccount -> Budget Income -> [Budget Expense] -> Abs Money -> [TransactionRow] -> [Advance] -> AccountHealth
analyzeWith now BankAccount {balance} pay bs spend _ advs =
    -- TODO Move these to functions so I can test them

    let payday   = Schedule.next (schedule $ budget pay) now
        paycheck = Scheduled payday pay

        dailys = Health.timeline now payday spend bs
        dailyBalances = Health.dailyBalances balance dailys
        current = Amount balance :: Amount Balance


        advance = listToMaybe advs
        advAmt = advanceAmount advance

        -- TODO how do we handle advances in the calculation? Let's assume we haven't sent it yet. It's promised, but not sent. But the minute we sent it we need to mark it as sent and calculate it differently, because their account isn't in jeopardy yet. Or we need some way to tell if it's actually hit (Using their transactions!)
        minimum = minBalance advAmt current dailyBalances

        bills = Health.billsDue dailys
        spendingTotal = Health.totalSpending dailys


        -- first date negative, including overdraft



    -- Not using transactions for now, simplify because we can't actually
    -- take any action if things are settling today. We can only move
    -- one day out
    in AccountHealth
        { balance = current
        , minimum
        , spendingDaily = spend
        , spendingTotal
        , dailyBalances
        , advance
        , paycheck
        , bills
        , billsTotal = totalBills bills
        , afterPaycheck = afterPaycheckBalance minimum pay advAmt
        , last = lastBalance advAmt dailyBalances
        , overdraft = predictedOverdraftDate (advanceAmount advance) dailyBalances
        }




totalBills :: [Scheduled (Budget Expense)] -> Abs Money
totalBills bills =
   absolute $ List.sum $ List.map (value . Budget.amount . budget . Scheduled.item) bills


-- it'd be nice to not mess up the balances and amounts

minBalance :: Amount Advance -> Amount Balance -> [DailyBalance] -> Amount Minimum
minBalance (Amount advAmt) (Amount balance) dailyBalances =
  Amount $ Health.minimumBalance balance dailyBalances + advAmt


predictedOverdraftDate :: Amount Advance -> [DailyBalance] -> Maybe Day
predictedOverdraftDate adv dbs =
   List.find (isOverdraft adv) dbs
     & fmap (Daily.date . Daily.daily)


isOverdraft :: Amount Advance -> DailyBalance -> Bool
isOverdraft (Amount adv) db = (Daily.balance db + adv) < 0


advanceAmount :: Maybe Advance -> Amount Advance
advanceAmount advance =
  Amount $ fromMaybe (Money.fromFloat 0) $ fmap Advances.amount advance


afterPaycheckBalance :: Amount Minimum -> Budget Income -> Amount Advance -> Amount After
afterPaycheckBalance (Amount min) pay (Amount adv) =
  Amount $ min + (value $ Budget.amount $ budget pay) - adv


lastBalance :: Amount Advance -> [DailyBalance] -> Amount Last
lastBalance (Amount adv) dbs =
  Amount $ (Daily.balance $ List.last dbs) + adv


primaryIncome
  :: ( MonadEffects '[Throw Error] m )
  => Guid Account -> [Budget Income] -> m (Budget Income)
primaryIncome accountId pays =
  pays
    & List.sortOn (value . Budget.amount . budget)
    & listToMaybe
    & required (NoIncome accountId)



required :: MonadEffect (Throw Error) m => Error -> Maybe a -> m a
required = required' id


required' :: MonadEffect (Throw Error) m => (a -> Maybe b) -> Error -> a -> m b
required' f e ma =
  case f ma of
    Nothing -> throwSignal e
    Just b  -> pure b
