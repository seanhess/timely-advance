{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AccountUpdate where

-- TODO real transaction analysis
import           Control.Effects                   (MonadEffects)
import           Control.Effects.Log               (Log)
import qualified Control.Effects.Log               as Log
import           Control.Effects.Signal            (Throw, throwSignal)
import qualified Control.Effects.Signal            as Signal
import           Control.Effects.Time              (Time, UTCTime)
import qualified Control.Effects.Time              as Time
import           Control.Monad                     (when)
import           Control.Monad.Catch               (MonadThrow (..))
import           Data.Function                     ((&))
import qualified Data.List                         as List
import qualified Data.Maybe                        as Maybe
import           Data.Model.Guid                   as Guid
import           Data.Model.Id                     (Id)
import           Data.Model.Money                  (Money)
import           Data.Model.Types                  (Phone)
import           Data.Model.Valid                  as Valid
import           Data.Number.Abs                   (Abs (value))
import           Data.Time.Calendar                (Day)
import qualified Network.AMQP.Worker               as Worker (Queue, topic)
import           Timely.Accounts                   (Accounts, TransactionRow (transactionId))
import qualified Timely.Accounts                   as Accounts
import           Timely.Accounts.Budgets           (Budgets)
import qualified Timely.Accounts.Budgets           as Budgets
import           Timely.Accounts.Types             (Account (..), BankAccount (bankAccountId))
import qualified Timely.Accounts.Types.BankAccount as BankAccount
import qualified Timely.Accounts.Types.Transaction as Transaction
import qualified Timely.Actions.AccountHealth      as AccountHealth
import           Timely.Advances                   (Advance, Advances)
import qualified Timely.Advances                   as Advances
import qualified Timely.App                        as App
import           Timely.Bank                       (Access, Banks, Token)
import qualified Timely.Bank                       as Bank
import           Timely.Evaluate.Health            (Income)
import           Timely.Evaluate.Health.Budget     (Budget (..))
import           Timely.Evaluate.Offer             (Projection (..))
import qualified Timely.Evaluate.Offer             as Offer
import qualified Timely.Evaluate.Schedule          as Schedule
import           Timely.Events                     as Events
import           Timely.Notify                     (Notify)
import qualified Timely.Notify                     as Notify
import           Timely.Transfers.Account          (TransferAccount)
import           Timely.Types.AccountHealth        (AccountHealth (..))
import           Timely.Types.Update               (Error (..))


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
      -- TODO mark the account as having an error?
      -- no, we don't have a way of storing it right now, unlike onboarding
      throwM err



-- I don't have the budgets
accountUpdate
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Advances, Notify, Throw Error, Budgets] m)
  => Account -> m ()
accountUpdate account@(Account{ accountId, bankToken }) = do
    now    <- Time.currentTime
    today  <- Time.currentDate

    check  <- bankBalances accountId bankToken now
    trans  <- updateTransactions accountId bankToken (bankAccountId check) today

    Log.debug ("trans", length trans)
    inc    <- Budgets.income accountId >>= require (NoIncome accountId)
    exs    <- Budgets.expenses accountId

    let health = AccountHealth.analyzeWith today check inc exs trans

    checkAdvance account health now today inc









checkAdvance
  :: ( MonadEffects '[Log, Advances, Notify] m
     )
  => Account -> AccountHealth -> UTCTime -> Day -> Budget Income -> m ()
checkAdvance Account {accountId, transferId, phone, credit} health now today inc = do
    offer  <- Advances.findOffer  accountId
    active <- Advances.findActive accountId

    let proj = Projection { expenses = value $ budgeted health, available = balance health  }
    Log.debug ("Offer?", proj, Maybe.isJust offer, List.length active)

    when (Offer.isNeeded offer active proj now) $ do
      offerAdvance today accountId transferId phone inc credit proj active
      pure ()



offerAdvance
   :: ( MonadEffects '[Log, Advances, Notify] m)
   => Day -> Guid Account -> Id TransferAccount -> Valid Phone -> Budget Income -> Money -> Projection -> [Advance] -> m Advance
offerAdvance today accountId transferId phone income credit health advances = do
    let dueNextPay = Schedule.next (schedule income)  today
    let amount = Offer.amount credit advances health
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
