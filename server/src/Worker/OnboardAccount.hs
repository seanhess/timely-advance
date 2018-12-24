{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Worker.OnboardAccount where


import Events (accountsNew, appExchange)
import qualified Endpoint.Accounts as Accounts
import Types.Account (Account(..))
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
    let queue = Worker.topic accountsNew "app.onboardAccount"
    Worker.bindQueue queue
    Worker.worker def queue onError onboardAccount



onboardAccount :: (MonadSelda m, MonadWorker m) => Message Account -> m ()
onboardAccount m = do
    let account = value m
    liftIO $ putStrLn "NEW ACCOUNT :)"
    liftIO $ print account

    liftIO $ getLine

    Accounts.saveBankBalance (accountId account) 200
    liftIO $ putStrLn " - done"

    -- wait for a minute, then update the bank balance :)


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
    , exchange :: Worker.Exchange
    }

type WorkerM = ReaderT AppState IO

instance MonadSelda WorkerM where
    seldaConnection = asks db

instance MonadWorker WorkerM where
    amqpConnection = asks amqp
    amqpExchange = asks exchange


run :: IO ()
run = do
    conn <- Worker.connect (Worker.fromURI "amqp://guest:guest@localhost:5672")
    db <- pgOpen $ PGConnectInfo "localhost" 5432 "postgres" Nothing (Just "postgres") Nothing
    let state = AppState db conn appExchange
    putStrLn "Running worker"
    runReaderT connect state
