{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Worker.OnboardAccount where


import qualified Events
import qualified Accounts.Account as Account
import Types.Account (Account(..))
import Types.Account.Customer (Customer(..))
import Types.Account.Bank (Bank(..))
import Types.Application (Application(..))
import Control.Monad.Except (MonadError)
import Control.Exception (SomeException(..))
import Control.Monad.Effect (Effect(run))
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Plaid (MonadPlaid, runPlaid, PlaidError)
import qualified Control.Monad.Plaid as Plaid
import Control.Monad.IO.Class (liftIO)
import Database.Selda.PostgreSQL (pgOpen, PGConnectInfo(..))
import Network.AMQP.Worker (key, word, Key, Routing, WorkerException, def, Message(..))
import Network.AMQP.Worker (Queue)
import qualified Network.AMQP.Worker as Worker hiding (publish, bindQueue, worker)
import qualified Network.AMQP.Worker.Monad as Worker
import Network.AMQP.Worker.Monad (MonadWorker(..))
import qualified Network.Plaid as Plaid
import qualified Network.Plaid.Types as Plaid
import Database.Selda.Backend (SeldaConnection, MonadSelda(..), SeldaT, runSeldaT)
import Worker.WorkerM (WorkerM, loadState)



queue :: Queue Application
queue = Worker.topic Events.applicationsNew "app.onboardAccount"



-- TODO effects constraints, not this
handler :: (MonadSelda m, MonadWorker m, MonadPlaid m, MonadError PlaidError m) => Message Application -> m ()
handler m = do
    let app = value m
    liftIO $ putStrLn "NEW APPLICATION :)"
    liftIO $ print app

    -- create the customer record
    run $ Account.CreateCustomer (newCustomer app)


    -- TODO bank service

    -- TODO call plaid
    -- TODO access token
    creds <- Plaid.credentials
    res <- runPlaid $ Plaid.reqAccounts creds (plaidToken app)
    liftIO $ print $ res

    liftIO $ putStrLn " - done"


newCustomer :: Application -> Customer
newCustomer Application {..} = Customer {..}

-- newBank :: Application -> Bank
-- newBank Application {..} = Bank {..}
--   where
--     balance = 200
--     accessToken = "fake-access-token"









