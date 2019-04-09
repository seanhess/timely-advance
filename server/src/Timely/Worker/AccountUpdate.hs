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
import           Data.Maybe                        (fromMaybe)
import           Data.Model.Guid                   as Guid
import           Data.Model.Money                  (Money)
import           Data.Number.Abs                   (Abs (value))
import           Data.Time.Calendar                (Day)
import qualified Network.AMQP.Worker               as Worker (Queue, topic)
import           Timely.Accounts                   (Accounts, TransactionRow (transactionId))
import qualified Timely.Accounts                   as Accounts
import           Timely.Accounts.Budgets           (Budgets)
import           Timely.Accounts.Types             (Account (..), BankAccount (bankAccountId))
import qualified Timely.Accounts.Types             as Account (Account (..))
import qualified Timely.Accounts.Types.BankAccount as BankAccount
import qualified Timely.Accounts.Types.Transaction as Transaction
import           Timely.Advances                   (Advance, Advances)
import qualified Timely.Advances                   as Advances
import qualified Timely.Api.AccountHealth          as AccountHealth
import qualified Timely.App                        as App
import           Timely.Bank                       (Access, Banks, Token)
import qualified Timely.Bank                       as Bank
import qualified Timely.Evaluate.Offer             as Offer
import           Timely.Evaluate.Schedule          (DayOfMonth (..), Schedule (..))
import qualified Timely.Evaluate.Schedule          as Schedule
import           Timely.Events                     as Events
import           Timely.Notify                     (Notify)
import qualified Timely.Notify                     as Notify
import           Timely.Types.Health               (AccountHealth (..))
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
      -- Applications.markResultOnboarding accountId Error
      throwM err



-- I don't have the budgets
accountUpdate
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Advances, Notify, Throw Error, Budgets] m
     )
  => Account -> m ()
accountUpdate account@(Account{ accountId, bankToken }) = do
    now      <- Time.currentTime
    today    <- Time.currentDate

    checking <- bankBalances accountId bankToken now
    _        <- updateTransactions accountId bankToken checking today

    health   <- AccountHealth.analyze accountId

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
    let projection = Offer.Projection (balance health) (value $ budgeted health)
    pure $ (Offer.isNeeded offer active projection now)



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




-- | Synchronizes the last 30 days of transactions with the bank
updateTransactions
  :: ( MonadEffects '[Banks, Log, Accounts] m )
  => Guid Account -> Token Bank.Access -> BankAccount -> Day -> m [TransactionRow]
updateTransactions accountId token check today = do

    let numDays = 30
    tsb   <- fromBank <$> Bank.loadTransactionsDays token (bankAccountId check) numDays today
    tsa   <- Accounts.transDays accountId numDays today

    let tsNew = List.deleteFirstsBy eqTransId tsb tsa
    Log.debug ("new transactions", List.length tsNew)
    Accounts.transSave accountId tsNew

    pure $ List.unionBy eqTransId tsa tsb


  where
    eqTransId t1 t2 = transactionId t1 == transactionId t2

    fromBank = List.map (Transaction.fromBank accountId)






require err Nothing = throwSignal err
require _ (Just a)  = pure a
