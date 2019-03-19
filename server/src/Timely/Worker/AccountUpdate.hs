{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AccountUpdate where

-- TODO real transaction analysis
import           Control.Effects               (MonadEffects)
import           Control.Effects.Log           (Log)
import qualified Control.Effects.Log           as Log
import           Control.Effects.Signal        (Throw, throwSignal)
import qualified Control.Effects.Signal        as Signal
import           Control.Effects.Time          (Time)
import qualified Control.Effects.Time          as Time
import           Control.Exception             (Exception)
import           Control.Monad                 (when)
import           Control.Monad.Catch           (MonadThrow (..))
import           Data.Function                 ((&))
import qualified Data.List                     as List
import           Data.Model.Guid               as Guid
import           Data.Model.Money              (Money)
import           Data.Time.Calendar            (Day)
import qualified Network.AMQP.Worker           as Worker (Queue, topic)
import           Timely.AccountStore.Account   (Accounts)
import qualified Timely.AccountStore.Account   as Accounts
import           Timely.AccountStore.Types     (Account, AccountRow (..), BankAccount)
import qualified Timely.AccountStore.Types     as BankAccount
import qualified Timely.AccountStore.Types     as Account (AccountRow (..))
import           Timely.Advances               (Advance, Advances)
import qualified Timely.Advances               as Advances
import qualified Timely.App                    as App
import           Timely.Bank                   (Access, Banks, Token)
import qualified Timely.Bank                   as Bank
import qualified Timely.Evaluate.AccountHealth as AccountHealth
import qualified Timely.Evaluate.Offer         as Offer
import qualified Timely.Evaluate.Paydate       as Paydate
import           Timely.Evaluate.Types         (Projection (..))
import           Timely.Events                 as Events
import           Timely.Notify                 (Notify)
import qualified Timely.Notify                 as Notify



queue :: Worker.Queue AccountRow
queue = Worker.topic Events.transactionsUpdate "app.account.update"


start :: IO ()
start = App.start queue handler


handler
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Advances, Notify] m
     , MonadThrow m
     )
  => AccountRow -> m ()
handler account = do
    Log.context $ Guid.toText (accountId account)
    Log.info "AccountUpdate"
    accountUpdate account
      & Signal.handleException onError

  where
    onError :: (MonadThrow m) => UpdateError -> m ()
    onError err =
      -- TODO mark the account as having an error?
      -- Applications.markResultOnboarding accountId Error
      throwM err



accountUpdate
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Advances, Notify, Throw UpdateError] m
     )
  => AccountRow -> m ()
accountUpdate account@(AccountRow{ accountId, bankToken }) = do

    checking <- updateBankBalances accountId bankToken
                  >>= require MissingChecking

    health <- updateHealth accountId checking

    Log.debug ("Health", health)
    checkAdvance account health

    pure ()

  where

    require err Nothing = throwSignal (err accountId)
    require _ (Just a)  = pure a



checkAdvance
  :: ( MonadEffects '[Time, Log, Advances, Notify] m
     )
  => AccountRow -> Projection -> m ()
checkAdvance account health = do
    now    <- Time.currentTime
    offer  <- Advances.findOffer  (accountId account)
    active <- Advances.findActive (accountId account)
    when (Offer.isNeeded offer active health now) $ do
      a <- offerAdvance account Offer.amount (Time.utctDay now)
      Log.debug ("advance", a)
      pure ()


offerAdvance
   :: ( MonadEffects '[Log, Advances, Notify] m)
   => AccountRow -> Money -> Day -> m Advance
offerAdvance account amount today = do
    let id = accountId account
        transactions = []
        frequency    = Paydate.frequency transactions
        nextPayday   = Paydate.next frequency today
        due          = nextPayday
    advance <- Advances.create id (Account.transferId account) amount due
    Notify.send account (Notify.Message (Advances.advanceId advance) Notify.Advance message)
    pure advance
  where
    message = "Your bank balance is getting low. Click here to accept an advance from Timely"





updateHealth
  :: ( MonadEffects '[Accounts] m)
  => Guid Account -> BankAccount -> m Projection
updateHealth accountId checking = do
    let health = AccountHealth.analyze (BankAccount.balance checking)
    Accounts.setHealth accountId health
    pure health



-- | updates the bank accounts and returns the checking account
updateBankBalances
    :: ( MonadEffects '[Time, Accounts, Banks] m)
    => Guid Account -> Token Access -> m (Maybe BankAccount)
updateBankBalances accountId token = do
    now <- Time.currentTime
    banks <- Bank.loadAccounts token
    let accounts = List.map (BankAccount.toBankAccount accountId now) banks
    Accounts.setBanks accountId accounts
    pure $ List.find BankAccount.isChecking accounts



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
