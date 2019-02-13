{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AccountUpdate where

-- TODO real transaction analysis
import           Control.Exception             (Exception)
import           Control.Monad                 (when)
import           Control.Monad.Catch           (MonadThrow (..))
import           Control.Monad.Log             (MonadLog)
import qualified Control.Monad.Log             as Log
import           Control.Monad.Service         (Service (run))
import qualified Data.List                     as List
import           Data.Model.Guid               as Guid
import           Data.Model.Money              (Money)
import           Data.Time.Calendar            (Day)
import           Network.AMQP.Worker           (MonadWorker)
import qualified Network.AMQP.Worker           as Worker hiding (publish)
import qualified Network.AMQP.Worker.Monad     as Worker
import           Timely.AccountStore.Account   (AccountStore)
import qualified Timely.AccountStore.Account   as AccountStore
import           Timely.AccountStore.Types     (Account (bankToken), BankAccount (balance), isChecking, toBankAccount)
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
import           Timely.Time                   (Time)
import qualified Timely.Time                   as Time
import           Timely.Types.Private          (Private (..))



queue :: Worker.Queue (Guid Account)
queue = Worker.topic Events.transactionsNew "app.account.update"



-- | Schedules all accounts for an update
schedule
  :: ( Service m AccountStore
     , MonadThrow m
     , MonadWorker m
     )
  => m ()
schedule = do
    -- for now, update all accounts every hour
    accounts <- run $ AccountStore.All
    mapM scheduleAccountUpdate accounts
    pure ()
  where
    scheduleAccountUpdate account = do
      -- liftIO $ putStrLn $ "ACCOUNT: " ++ (show $ accountId account)
      Worker.publish Events.transactionsNew (AccountRow.accountId account)



handler
  :: ( Service m Banks
     , Service m AccountStore
     , Service m Advances
     , Service m Time
     , Service m Notify
     , MonadThrow m
     , MonadLog m
     )
  => Guid Account -> m ()
handler accountId = do

    Log.context (Guid.toText accountId)
    Log.info "AccountUpdate"

    account  <- run (AccountStore.Find accountId)
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
  :: ( Service m Advances
     , Service m Time
     , Service m Notify
     , MonadLog m
     )
  => Account -> Projection -> m ()
checkAdvance account health = do
    now    <- run $ Time.CurrentTime
    offer  <- run $ Advances.FindOffer  (Account.accountId account)
    active <- run $ Advances.FindActive (Account.accountId account)
    when (Offer.isNeeded offer active health now) $ do
      a <- offerAdvance account Offer.amount (Time.utctDay now)
      Log.debug ("advance", a)
      pure ()


offerAdvance
   :: ( Service m Advances
      , Service m Notify
      , MonadLog m
      )
   => Account -> Money -> Day -> m Advance
offerAdvance account amount today = do
    let id = Account.accountId account
        transactions = []
        frequency    = Paydate.frequency transactions
        nextPayday   = Paydate.next frequency today
        due          = nextPayday
    advance <- run $ Advances.Create id amount due
    run $ Notify.Send account (Notify.Message (Advances.advanceId advance) Notify.Advance message)
    pure advance
  where
    message = "Your bank balance is getting low. Click here to accept an advance from Timely"





updateHealth
  :: ( Service m AccountStore)
  => Guid Account -> BankAccount -> m Projection
updateHealth accountId checking = do
    let health = AccountHealth.analyze (balance checking)
    run $ AccountStore.SetHealth accountId health
    pure health



-- | updates the bank accounts and returns the checking account
updateBankBalances
    :: ( Service m Banks
       , Service m AccountStore
       )
    => Guid Account -> Token Access -> m (Maybe BankAccount)
updateBankBalances accountId token = do
    banks <- run $ Bank.LoadAccounts token
    let accounts = List.map (toBankAccount accountId) banks
    run $ AccountStore.SetBankAccounts accountId accounts
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
