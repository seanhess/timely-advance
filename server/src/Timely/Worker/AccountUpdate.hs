{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AccountUpdate where

import           Control.Effects                    (MonadEffects)
import           Control.Effects.Log                (Log)
import qualified Control.Effects.Log                as Log
import           Control.Effects.Signal             (Throw, throwSignal)
import qualified Control.Effects.Signal             as Signal
import           Control.Effects.Time               (Time, UTCTime)
import qualified Control.Effects.Time               as Time
import           Control.Monad.Catch                (MonadThrow (..))
import           Data.Function                      ((&))
import qualified Data.List                          as List
import qualified Data.Maybe                         as Maybe
import           Data.Model.Guid                    as Guid
import           Data.Model.Id                      (Id)
import qualified Data.Model.Meta                    as Meta
import           Data.Model.Money                   (Money)
import           Data.Model.Types                   (Phone)
import           Data.Model.Valid                   (Valid)
import           Data.Number.Abs                    (Abs (value))
import           Data.Time.Calendar                 (Day)
import qualified Network.AMQP.Worker                as Worker (Queue, topic)
import           Timely.Accounts                    (Accounts, TransactionRow (transactionId))
import qualified Timely.Accounts                    as Accounts
import           Timely.Accounts.Budgets            (BudgetMeta, Budgets)
import qualified Timely.Accounts.Budgets            as Budgets
import           Timely.Accounts.Types              (Account (..), BankAccount (bankAccountId))
import qualified Timely.Accounts.Types.BankAccount  as BankAccount
import qualified Timely.Accounts.Types.Transaction  as Transaction
import           Timely.Actions.AccountHealth       (AccountHealth (..))
import qualified Timely.Actions.AccountHealth       as AccountHealth
import           Timely.Advances                    (Advance, Advances)
import qualified Timely.Advances                    as Advances
import qualified Timely.App                         as App
import           Timely.Bank                        (Access, Banks, Token)
import qualified Timely.Bank                        as Bank
import           Timely.Evaluate.Health.Budget      (Budget (..))
import           Timely.Evaluate.Health.Timeline    (Timeline (..))
import           Timely.Evaluate.Health.Transaction (Income)
import qualified Timely.Evaluate.Offer              as Offer
import qualified Timely.Evaluate.Schedule           as Schedule
import           Timely.Events                      as Events
import           Timely.Notify                      (Notify)
import qualified Timely.Notify                      as Notify
import           Timely.Transfers.Account           (TransferAccount)
import           Timely.Types.Update                (Error (..))


queue :: Worker.Queue Account
queue = Worker.topic Events.transactionsUpdate "app.account.update"


start :: IO ()
start = App.start queue handler


handler
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Advances, Notify, Budgets] m
     , MonadThrow m
     )
  => Account -> m ()
handler account = do
    Log.context $ Guid.toText (accountId account)
    Log.info "AccountUpdate"
    accountUpdate account
      & Signal.handleException onError

  where
    onError :: (MonadThrow m) => Error -> m ()
    onError err =
      -- Should we mark the account as having an error?
      -- no, we don't have a way of storing it right now, unlike onboarding
      throwM err



accountUpdate
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Advances, Notify, Throw Error, Budgets] m)
  => Account -> m ()
accountUpdate account@(Account{ accountId, bankToken }) = do
    now    <- Time.currentTime
    today  <- Time.currentDate

    check  <- bankBalances accountId bankToken now
    trans  <- updateTransactions accountId bankToken (bankAccountId check) today

    Log.debug ("trans", length trans)
    pays   <- Budgets.getIncomes accountId
    bills  <- Budgets.getExpenses accountId

    let health = AccountHealth.analyzeWith today check pays bills trans

    pay    <- primaryIncome accountId pays
    checkAdvance account (timeline health) now today pay






primaryIncome
  :: ( MonadEffects '[Throw Error] m )
  => Guid Account -> [BudgetMeta Income] -> m (Budget Income)
primaryIncome accountId pays =
  pays
    & List.map Meta.value
    & List.sortOn (value . amount)
    & Maybe.listToMaybe
    & require (NoIncome accountId)




checkAdvance
  :: ( MonadEffects '[Log, Advances, Notify] m
     )
  => Account -> Timeline -> UTCTime -> Day -> Budget Income -> m ()
checkAdvance Account {accountId, transferId, phone, credit} timeline now today pay = do
    offer  <- Advances.findOffer  accountId
    active <- Advances.findActive accountId

    case Offer.check credit offer active (lowest timeline) now of
      Nothing -> pure ()
      Just amount -> do
        offerAdvance today accountId transferId phone pay amount
        pure ()




offerAdvance
   :: ( MonadEffects '[Log, Advances, Notify] m)
   => Day -> Guid Account -> Id TransferAccount -> Valid Phone -> Budget Income -> Abs Money -> m Advance
offerAdvance today accountId transferId phone income amount = do
    let dueNextPay = Schedule.next (schedule income)  today
    advance <- Advances.create accountId transferId amount dueNextPay
    Notify.send accountId phone (Notify.Message (Advances.advanceId advance) Notify.Advance message)
    Log.debug ("advance", advance)
    pure advance
  where
    message = "Your bank balance is getting low. Click here to accept an advance from Timely"




-- | updates the bank accounts and returns the checking account
bankBalances
    :: ( MonadEffects '[Accounts, Banks, Throw Error] m)
    => Guid Account -> Token Access -> UTCTime -> m BankAccount
bankBalances accountId token now = do
    banks <- Bank.loadAccounts token
    let accounts = List.map (BankAccount.toBankAccount accountId now) banks
    Accounts.setBanks accountId accounts

    List.find BankAccount.isChecking accounts
      & require (NoChecking accountId)




-- | Synchronizes the last 30 days of transactions with the bank
updateTransactions
  :: ( MonadEffects '[Banks, Log, Accounts] m )
  => Guid Account -> Token Bank.Access -> Id Bank.Account -> Day -> m [TransactionRow]
updateTransactions accountId token checkId today = do
    let numDays = 30
    tsb   <- fromBank <$> Bank.loadTransactionsDays token checkId numDays today
    tsa   <- Accounts.transDays accountId numDays today
    Accounts.transSave accountId $ transNew tsb tsa
    pure $ transAll tsb tsa


  where
    fromBank = List.map (Transaction.fromBank accountId)



transNew :: [TransactionRow] -> [TransactionRow] -> [TransactionRow]
transNew bank account =
    List.deleteFirstsBy eqTransId bank account


transAll :: [TransactionRow] -> [TransactionRow] -> [TransactionRow]
transAll bank account =
    List.unionBy eqTransId bank account


eqTransId t1 t2 = transactionId t1 == transactionId t2



require err Nothing = throwSignal err
require _ (Just a)  = pure a
