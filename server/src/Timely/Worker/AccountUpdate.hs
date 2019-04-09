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
import           Data.Maybe                        (fromMaybe, mapMaybe)
import           Data.Model.Guid                   as Guid
import           Data.Model.Money                  (Money)
import           Data.Time.Calendar                (Day)
import qualified Network.AMQP.Worker               as Worker (Queue, topic)
import           Timely.Accounts                   (Accounts, TransactionRow (transactionId))
import qualified Timely.Accounts                   as Accounts
import           Timely.Accounts.Budgets           (Budgets)
import           Timely.Accounts.Types             (Account (..), BankAccount (balance, bankAccountId))
import qualified Timely.Accounts.Types             as Account (Account (..))
import qualified Timely.Accounts.Types.BankAccount as BankAccount
import qualified Timely.Accounts.Types.Transaction as Transaction
import           Timely.Advances                   (Advance, Advances)
import qualified Timely.Advances                   as Advances
import qualified Timely.Api.AccountHealth          as AccountHealth
import           Timely.Api.Transactions           (toIncome)
import qualified Timely.App                        as App
import           Timely.Bank                       (Access, Banks, Token)
import qualified Timely.Bank                       as Bank
import           Timely.Evaluate.Health            (AccountHealth, Budget, Income, Transaction)
import qualified Timely.Evaluate.Health            as Health
import qualified Timely.Evaluate.Offer             as Offer
import           Timely.Evaluate.Schedule          (DayOfMonth (..), Schedule (..))
import qualified Timely.Evaluate.Schedule          as Schedule
import           Timely.Events                     as Events
import           Timely.Notify                     (Notify)
import qualified Timely.Notify                     as Notify
import           Timely.Types.Update               (Error (..))


queue :: Worker.Queue (Account, Int)
queue = Worker.topic Events.transactionsUpdate "app.account.update"


start :: IO ()
start = App.start queue handler


handler
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Advances, Notify, Budgets] m
     , MonadThrow m
     )
  => (Account, Int) -> m ()
handler (account, numTransactions) = do
    Log.context $ Guid.toText (accountId account)
    Log.info "AccountUpdate"
    accountUpdate account numTransactions
      & Signal.handleException onError

  where
    onError :: (MonadThrow m) => Error -> m ()
    onError err =
      -- TODO mark the account as having an error?
      -- Applications.markResultOnboarding accountId Error
      throwM err



-- I don't have the budgets
accountUpdate
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Advances, Notify, Throw Error, Budgets] m
     )
  => Account -> Int -> m ()
accountUpdate account@(Account{ accountId, bankToken }) numTransactions = do
    now      <- Time.currentTime
    today    <- Time.currentDate

    checking <- bankBalances accountId bankToken now
    trans    <- updateTransactions accountId bankToken checking numTransactions today

    health   <- AccountHealth.analyze accountId

    -- TODO calculate health based on transactions and checking balance :)

    isOffer  <- checkAdvance account health now
    when isOffer $ do
      offerAdvance account Offer.amount (Time.utctDay now)
      pure ()






checkAdvance
  :: ( MonadEffects '[Log, Advances, Notify] m
     )
  => Account -> AccountHealth -> UTCTime -> m Bool
checkAdvance account health now = do
    offer  <- Advances.findOffer  (accountId account)
    active <- Advances.findActive (accountId account)
    pure $ (Offer.isNeeded offer active health now)



offerAdvance
   :: ( MonadEffects '[Log, Advances, Notify] m)
   => Account -> Money -> Day -> m Advance
offerAdvance account amount today = do
    let id = accountId account
        transactions = []
        schedule     = fromMaybe (Monthly (DayOfMonth 1)) $ Schedule.schedule transactions
        nextPayday   = Schedule.next schedule today
        due          = nextPayday
    advance <- Advances.create id (Account.transferId account) amount due
    Notify.send account (Notify.Message (Advances.advanceId advance) Notify.Advance message)
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
      & require (MissingChecking accountId)




-- | Load the last n transactions from the bank. Check to see if any are already saved, and save the rest
-- TODO syncronoize, instead of relying on the number
updateTransactions
  :: ( MonadEffects '[Banks, Log, Accounts] m )
  => Guid Account -> Token Bank.Access -> BankAccount -> Int -> Day -> m [TransactionRow]
updateTransactions accountId bankToken checking numTransactions today = do
    Log.info "update transactions"
    ts <- Bank.loadTransactions bankToken (bankAccountId checking) $
            Bank.limitLast today numTransactions

    let tsNew = List.map (Transaction.fromBank accountId) ts
    tsOld <- Accounts.listTransactions accountId 0 numTransactions
    let tsSave = List.deleteFirstsBy eqTransId tsNew tsOld
    Log.debug ("new transactions", List.length tsSave)
    Accounts.saveTransactions accountId tsSave
    ERROR "this should return all recent transactions. We need it for analysis anyway, so just load all of them and synchronize!"
    pure _

  where
    eqTransId t1 t2 = transactionId t1 == transactionId t2






require err Nothing = throwSignal err
require _ (Just a)  = pure a
