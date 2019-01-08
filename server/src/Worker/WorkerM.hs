{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Worker.WorkerM where

import Config (loadEnv, Env(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Plaid (MonadPlaid, runPlaid)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Exception (SomeException)
import qualified Control.Monad.Plaid
import Data.Aeson (FromJSON)
import Data.String.Conversions (cs)
import qualified Database.Selda.PostgreSQL as Selda
import Database.Selda (MonadMask(..))
import Database.Selda.Backend (SeldaConnection, MonadSelda(..), SeldaT, runSeldaT)
import Network.AMQP.Worker (Queue, WorkerException, def, Message)
import qualified Network.AMQP.Worker as Worker hiding (publish, bindQueue, worker)
import qualified Network.AMQP.Worker.Monad as Worker
import Network.AMQP.Worker.Monad (MonadWorker(..))
import qualified Network.Plaid as Plaid
import qualified Network.Plaid.Types as Plaid
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP


data AppState = AppState
    { dbConn :: SeldaConnection
    , amqpConn :: Worker.Connection
    , plaid :: Plaid.Credentials
    , manager :: HTTP.Manager
    , env :: Env
    }

type WorkerM = ReaderT AppState IO

instance MonadSelda WorkerM where
    seldaConnection = asks dbConn

instance MonadWorker WorkerM where
    amqpConnection = asks amqpConn

instance MonadPlaid WorkerM where
    credentials = asks plaid
    manager = asks manager
    baseUrl = asks (plaidBaseUrl . env)


loadState :: (MonadIO m, MonadMask m) => m AppState
loadState = do
    env <- loadEnv
    amqpConn <- Worker.connect (Worker.fromURI $ cs $ amqp env)
    dbConn <- Selda.pgOpen' Nothing (cs $ postgres env)
    manager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
    let plaid = Plaid.Credentials (plaidClientId env) (plaidClientSecret env)
    pure $ AppState {..}


connect :: (FromJSON a, MonadWorker m) => Queue a -> (Message a -> m ()) -> m ()
connect queue handler = do
    Worker.bindQueue queue
    Worker.worker def queue onError handler



start :: (FromJSON a) => Queue a -> (Message a -> WorkerM ()) -> IO ()
start queue handler = do
    state <- loadState
    putStrLn "Running worker"
    print queue
    runReaderT (connect queue handler) state




-- standardized error handling
onError :: MonadWorker m => WorkerException SomeException -> m ()
onError e = do
    liftIO $ putStrLn "Do something with errors"
    liftIO $ print e
    -- TODO handle errors. Create an error queue?
    -- TODO send to rollbar or somewhere similar

