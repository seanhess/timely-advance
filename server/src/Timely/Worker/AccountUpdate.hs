{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AccountUpdate where

-- TODO real transaction analysis
import           Control.Effects                  (MonadEffects)
import           Control.Effects.Log              (Log)
import qualified Control.Effects.Log              as Log
import           Control.Effects.Signal           (Throw, throwSignal)
import qualified Control.Effects.Signal           as Signal
import           Control.Effects.Time             (Time, UTCTime)
import qualified Control.Effects.Time             as Time
import           Control.Exception                (Exception)
import           Control.Monad                    (when)
import           Control.Monad.Catch              (MonadThrow (..))
import           Data.Function                    ((&))
import qualified Data.List                        as List
import           Data.Maybe                       (fromMaybe)
import           Data.Model.Guid                  as Guid
import           Data.Model.Money                 (Money)
import           Data.Time.Calendar               (Day)
import qualified Network.AMQP.Worker              as Worker (Queue, topic)
import           Timely.AccountStore.Account      (Accounts)
import qualified Timely.AccountStore.Account      as Accounts
import           Timely.AccountStore.Transactions (Transaction (transactionId), Transactions)
import qualified Timely.AccountStore.Transactions as Transactions
import           Timely.AccountStore.Types        (Account, AccountRow (..), BankAccount (bankAccountId))
import qualified Timely.AccountStore.Types        as BankAccount
import qualified Timely.AccountStore.Types        as Account (AccountRow (..))
import           Timely.Advances                  (Advance, Advances)
import qualified Timely.Advances                  as Advances
import qualified Timely.App                       as App
import           Timely.Bank                      (Access, Banks, Token)
import qualified Timely.Bank                      as Bank
import qualified Timely.Evaluate.AccountHealth    as AccountHealth
import qualified Timely.Evaluate.Offer            as Offer
import           Timely.Evaluate.Schedule         (DayOfMonth (..), Schedule (..))
import qualified Timely.Evaluate.Schedule         as Schedule
import           Timely.Evaluate.Types            (Projection (..))
import           Timely.Events                    as Events
import           Timely.Notify                    (Notify)
import qualified Timely.Notify                    as Notify



queue :: Worker.Queue (AccountRow, Int)
queue = Worker.topic Events.transactionsUpdate "app.account.update"


start :: IO ()
start = App.start queue handler


handler
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Advances, Notify, Transactions] m
     , MonadThrow m
     )
  => (AccountRow, Int) -> m ()
handler (account, numTransactions) = do
    Log.context $ Guid.toText (accountId account)
    Log.info "AccountUpdate"
    accountUpdate account numTransactions
      & Signal.handleException onError

  where
    onError :: (MonadThrow m) => UpdateError -> m ()
    onError err =
      -- TODO mark the account as having an error?
      -- Applications.markResultOnboarding accountId Error
      throwM err



accountUpdate
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Advances, Notify, Throw UpdateError, Transactions] m
     )
  => AccountRow -> Int -> m ()
accountUpdate account@(AccountRow{ accountId, bankToken }) numTransactions = do
    now      <- Time.currentTime
    let today = Time.utctDay now

    checking <- bankBalances accountId bankToken now
    health   <- updateHealth accountId checking
    _        <- updateTransactions accountId bankToken checking numTransactions today

    isOffer  <- checkAdvance account health now
    when isOffer $ do
      offerAdvance account Offer.amount (Time.utctDay now)
      pure ()






checkAdvance
  :: ( MonadEffects '[Log, Advances, Notify] m
     )
  => AccountRow -> Projection -> UTCTime -> m Bool
checkAdvance account health now = do
    offer  <- Advances.findOffer  (accountId account)
    active <- Advances.findActive (accountId account)
    pure $ (Offer.isNeeded offer active health now)



offerAdvance
   :: ( MonadEffects '[Log, Advances, Notify] m)
   => AccountRow -> Money -> Day -> m Advance
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





updateHealth
  :: ( MonadEffects '[Accounts, Log] m)
  => Guid Account -> BankAccount -> m Projection
updateHealth accountId checking = do
    let health = AccountHealth.analyze (BankAccount.balance checking)
    Accounts.setHealth accountId health
    Log.debug ("Health", health)
    pure health



-- | updates the bank accounts and returns the checking account
bankBalances
    :: ( MonadEffects '[Accounts, Banks, Throw UpdateError] m)
    => Guid Account -> Token Access -> UTCTime -> m BankAccount
bankBalances accountId token now = do
    banks <- Bank.loadAccounts token
    let accounts = List.map (BankAccount.toBankAccount accountId now) banks
    Accounts.setBanks accountId accounts

    List.find BankAccount.isChecking accounts
      & require (MissingChecking accountId)




-- | Load the last n transactions from the bank. Check to see if any are already saved, and save the rest
updateTransactions
  :: ( MonadEffects '[Banks, Log, Transactions] m )
  => Guid Account -> Token Bank.Access -> BankAccount -> Int -> Day -> m ()
updateTransactions accountId bankToken checking numTransactions today = do
    Log.info "update transactions"
    ts <- Bank.loadTransactions bankToken (bankAccountId checking) $
            Bank.limitLast today numTransactions

    let tsNew = List.map (Transactions.fromBank accountId) ts
    tsOld <- Transactions.list accountId 0 numTransactions
    let tsSave = List.deleteFirstsBy eqTransId tsNew tsOld
    Log.debug ("new transactions", List.length tsSave)
    Transactions.save accountId tsSave

  where
    eqTransId t1 t2 = transactionId t1 == transactionId t2




-- TODO need a way to know if advances are pending or not. Do they have the money, or have we just SENT them money?

-- ASSUME: that we know if an advance is pending.. WE neet to mark them as pending / active / etc at some point (when we analyze their transactions, aka here)

-- ASSUME: daily... or on an interval we KNOW they will resolve. 2-3 days? erm... WE could scan in the middle of the night


-- DAY 1: they need money
-- DAY 2: (A) they still need money, it's on the way. pending advance. So they're healthy!
-- DAY 2: (B) things are worse, they need MORE money. We can send them another advance?
-- DAY 3: Advance arrives, they're healthy



-- findAccountByBankId
--   :: (MonadEffects '[Accounts, Throw Error] m)
--   => Id Plaid.Item -> m (Guid Account)
-- findAccountByBankId i = do
--   ma <- Accounts.findByBankId i
--   case ma of
--     (Just a) -> pure a
--     Nothing  -> throwSignal $ MissingBankId i


data UpdateError
    = MissingChecking (Guid Account)
    deriving (Show, Eq)

instance Exception UpdateError



require err Nothing = throwSignal err
require _ (Just a)  = pure a
