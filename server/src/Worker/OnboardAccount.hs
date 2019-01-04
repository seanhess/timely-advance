{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Worker.OnboardAccount where


import qualified Events
import qualified Accounts.Account as Account
import Types.Account (Account(..))
import Types.Account.Customer (Customer(..))
import Types.Account.Bank (Bank(..))
import Types.Application (Application(..))
import Control.Exception (SomeException(..))
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.IO.Class (liftIO)
import Database.Selda.PostgreSQL (pgOpen, PGConnectInfo(..))
import Network.AMQP.Worker (key, word, Key, Routing, WorkerException, def, Message(..))
import qualified Network.AMQP.Worker as Worker hiding (publish, bindQueue, worker)
import qualified Network.AMQP.Worker.Monad as Worker
import Network.AMQP.Worker.Monad (MonadWorker(..))
import Database.Selda.Backend (SeldaConnection, MonadSelda(..), SeldaT, runSeldaT)



connect :: (MonadSelda m, MonadWorker m) => m ()
connect = do
    let queue = Worker.topic Events.applicationsNew "app.onboardAccount"
    Worker.bindQueue queue
    Worker.worker def queue onError onboardAccount



onboardAccount :: (MonadSelda m, MonadWorker m) => Message Application -> m ()
onboardAccount m = do
    let app = value m
    liftIO $ putStrLn "NEW APPLICATION :)"
    liftIO $ print app

    -- create the customer record
    Account.createCustomer $ newCustomer app

    -- TODO call plaid
    Account.createBank $ newBank app

    liftIO $ putStrLn " - done"


newCustomer :: Application -> Customer
newCustomer Application {..} = Customer {..}

newBank :: Application -> Bank
newBank Application {..} = Bank {..}
  where
    balance = 200
    accessToken = "fake-access-token"


onError :: MonadWorker m => WorkerException SomeException -> m ()
onError e = do
    liftIO $ putStrLn "Do something with errors"
    liftIO $ print e

    -- TODO handle errors. Create an error queue?
    -- TODO send to rollbar or somewhere similar





-- TODO move this to a common function. Handle ENV, etc
data AppState = AppState
    { db :: SeldaConnection
    , amqp :: Worker.Connection
    }

type WorkerM = ReaderT AppState IO

instance MonadSelda WorkerM where
    seldaConnection = asks db

instance MonadWorker WorkerM where
    amqpConnection = asks amqp


run :: IO ()
run = do
    conn <- Worker.connect (Worker.fromURI "amqp://guest:guest@localhost:5672")
    db <- pgOpen $ PGConnectInfo "localhost" 5432 "postgres" Nothing (Just "postgres") Nothing
    let state = AppState db conn
    putStrLn "Running worker"
    runReaderT connect state
