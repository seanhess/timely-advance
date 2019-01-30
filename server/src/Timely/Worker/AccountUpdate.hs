{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AccountUpdate where

import           Control.Exception             (Exception)
import           Control.Monad.Except          (MonadError (..))
import           Control.Monad.IO.Class        (liftIO, MonadIO)
import           Control.Monad.Service         (Service (run))
import qualified Data.List                     as List
import           Network.AMQP.Worker           (MonadWorker)
import qualified Network.AMQP.Worker           as Worker hiding (publish)
import qualified Network.AMQP.Worker.Monad     as Worker

import           Timely.AccountStore.Account   (AccountStore)
import qualified Timely.AccountStore.Account   as AccountStore
import           Timely.AccountStore.Types     (Account (bankToken), AccountRow (accountId), BankAccount (balance),
                                                isChecking, toBankAccount)
import           Timely.Advances               (Advances)
import qualified Timely.Advances               as Advances
import           Timely.Bank                   (Access, Banks, Token)
import qualified Timely.Bank                   as Bank
import qualified Timely.Evaluate.AccountHealth as AccountHealth
import           Timely.Evaluate.Types         (Projection (..))
import           Timely.Events                 as Events
import           Timely.Types.Guid             (Guid)
import           Timely.Types.Private          (Private (..))


queue :: Worker.Queue (Guid Account)
queue = Worker.topic Events.accountsUpdate "app.account.update"



schedule
  :: ( Service m AccountStore
     , MonadError EvaluateError m
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
      liftIO $ putStrLn $ "ACCOUNT: " ++ (show $ accountId account)
      Worker.publish Events.accountsUpdate (accountId account)



handler
  :: ( Service m Banks
     , Service m AccountStore
     , Service m Advances
     , MonadIO m
     , MonadError EvaluateError m
     )
  => Guid Account -> m ()
handler accountId = do

    liftIO $ putStrLn $ "HELLO!"  ++ show accountId
    account  <- run (AccountStore.Find accountId)
                  >>= require MissingAccount

    checking <- updateBankBalances accountId (private $ bankToken account)
                  >>= require MissingChecking

    updateHealth accountId checking
    pure ()

  where

    require :: MonadError err m => (Guid Account -> err) -> (Maybe a) -> m a
    require err Nothing = throwError (err accountId)
    require _ (Just a)  = pure a



updateHealth
  :: ( Service m AccountStore
     , Service m Advances
     )
  => Guid Account -> BankAccount -> m Projection
updateHealth accountId checking = do
    advances <- run (Advances.FindActive accountId)
    let health = AccountHealth.analyze (balance checking) advances
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
