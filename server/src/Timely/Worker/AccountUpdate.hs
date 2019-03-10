{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AccountUpdate where

-- TODO real transaction analysis
import           Control.Effects               (MonadEffects)
import           Control.Effects.Log           (Log)
import qualified Control.Effects.Log           as Log
import           Control.Effects.Time          (Time)
import qualified Control.Effects.Time          as Time
import           Control.Effects.Worker        (Publish)
import qualified Control.Effects.Worker        as Worker
import           Control.Exception             (Exception)
import           Control.Monad                 (when)
import           Control.Monad.Catch           (MonadThrow (..))
import qualified Data.List                     as List
import           Data.Model.Guid               as Guid
import           Data.Model.Money              (Money)
import           Data.Time.Calendar            (Day)
import qualified Network.AMQP.Worker           as Worker (Queue, topic)
import           Timely.AccountStore.Account   (Accounts)
import qualified Timely.AccountStore.Account   as Accounts
import           Timely.AccountStore.Types     (Account (bankToken), AccountRow (accountId), BankAccount (balance),
                                                isChecking, toBankAccount)
import qualified Timely.AccountStore.Types     as Account (Account (..))
import qualified Timely.AccountStore.Types     as AccountRow (AccountRow (..))
import           Timely.Advances               (Advance, Advances)
import qualified Timely.Advances               as Advances
import           Timely.Bank                   (Access, Banks, Token)
import qualified Timely.Bank                   as Bank
import qualified Timely.Evaluate.AccountHealth as AccountHealth
import qualified Timely.Evaluate.Offer         as Offer
import qualified Timely.Evaluate.Paydate       as Paydate
import           Timely.Evaluate.Types         (Projection (..))
import           Timely.Events                 as Events
import           Timely.Notify                 (Notify)
import qualified Timely.Notify                 as Notify
import           Timely.Types.Private          (Private (..))



queue :: Worker.Queue (Guid Account)
queue = Worker.topic Events.transactionsNew "app.account.update"



-- | Schedules all accounts for an update
schedule
  :: ( MonadEffects '[Accounts, Log, Publish] m
     , MonadThrow m
     )
  => m ()
schedule = do
    Log.context "Schedule AccountUpdate"
    accounts <- Accounts.all
    mapM scheduleAccountUpdate accounts
    pure ()
  where
    scheduleAccountUpdate account = do
      Log.info $ Guid.toText $ accountId account
      Worker.publish Events.transactionsNew (AccountRow.accountId account)



handler
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Advances, Notify] m
     , MonadThrow m
     )
  => Guid Account -> m ()
handler accountId = do

    Log.context (Guid.toText accountId)
    Log.info "AccountUpdate"

    account  <- Accounts.find accountId
                  >>= require MissingAccount

    checking <- updateBankBalances accountId (private $ bankToken account)
                  >>= require MissingChecking

    health <- updateHealth accountId checking

    Log.debug ("Health", health)
    checkAdvance account health

    pure ()

  where

    require :: (MonadThrow m, Exception err) => (Guid Account -> err) -> (Maybe a) -> m a
    require err Nothing = throwM (err accountId)
    require _ (Just a)  = pure a



checkAdvance
  :: ( MonadEffects '[Time, Log, Advances, Notify] m
     )
  => Account -> Projection -> m ()
checkAdvance account health = do
    now    <- Time.currentTime
    offer  <- Advances.findOffer  (Account.accountId account)
    active <- Advances.findActive (Account.accountId account)
    when (Offer.isNeeded offer active health now) $ do
      a <- offerAdvance account Offer.amount (Time.utctDay now)
      Log.debug ("advance", a)
      pure ()


offerAdvance
   :: ( MonadEffects '[Log, Advances, Notify] m)
   => Account -> Money -> Day -> m Advance
offerAdvance account amount today = do
    let id = Account.accountId account
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
    let health = AccountHealth.analyze (balance checking)
    Accounts.setHealth accountId health
    pure health



-- | updates the bank accounts and returns the checking account
updateBankBalances
    :: ( MonadEffects '[Time, Accounts, Banks] m)
    => Guid Account -> Token Access -> m (Maybe BankAccount)
updateBankBalances accountId token = do
    now <- Time.currentTime
    banks <- Bank.loadAccounts token
    let accounts = List.map (toBankAccount accountId now) banks
    Accounts.setBanks accountId accounts
    pure $ List.find isChecking accounts



-- TODO need a way to know if advances are pending or not. Do they have the money, or have we just SENT them money?

-- ASSUME: that we know if an advance is pending.. WE neet to mark them as pending / active / etc at some point (when we analyze their transactions, aka here)

-- ASSUME: daily... or on an interval we KNOW they will resolve. 2-3 days? erm... WE could scan in the middle of the night


-- DAY 1: they need money
-- DAY 2: (A) they still need money, it's on the way. pending advance. So they're healthy!
-- DAY 2: (B) things are worse, they need MORE money. We can send them another advance?
-- DAY 3: Advance arrives, they're healthy


data EvaluateError
    = MissingAccount  (Guid Account)
    | MissingChecking (Guid Account)
    deriving (Show, Eq)

instance Exception EvaluateError
