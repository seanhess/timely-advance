{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Worker.WorkerM where

import Config (loadEnv, Env(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Plaid (MonadPlaid)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Exception (SomeException)
import Control.Monad.Selda (Selda(..))
import qualified Control.Monad.Plaid
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Aeson (FromJSON)
import Data.String.Conversions (cs)
import qualified Database.Selda.PostgreSQL as Selda
import Database.Selda (MonadMask)
import Database.Selda.Backend (SeldaConnection)
import Network.AMQP.Worker (Queue, WorkerException, def)
import qualified Network.AMQP.Worker as Worker hiding (publish, bindQueue, worker)
import qualified Network.AMQP.Worker.Monad as Worker
import Network.AMQP.Worker.Monad (MonadWorker(..))
import qualified Network.Plaid as Plaid
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP


data AppState = AppState
    { dbConn :: Pool SeldaConnection
    , amqpConn :: Worker.Connection
    , plaid :: Plaid.Credentials
    , manager :: HTTP.Manager
    , env :: Env
    }

type WorkerM = ReaderT AppState IO

instance Selda WorkerM where
    withConnection action = do
      pool <- asks dbConn
      Pool.withResource pool action

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
    dbConn <- liftIO $ Pool.createPool (createConn $ cs $ postgres env) destroyConn 1 5 3
    manager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
    let plaid = Plaid.Credentials (plaidClientId env) (plaidClientSecret env)
    pure AppState {..}
  where
    createConn = Selda.pgOpen' Nothing
    destroyConn = Selda.seldaClose


connect :: (FromJSON a, MonadWorker m) => Queue a -> (a -> m ()) -> m ()
connect queue handler = do
    Worker.bindQueue queue
    Worker.worker def queue onError onMessage
  where
    onMessage m =
      handler (Worker.value m)


start :: (FromJSON a) => Queue a -> (a -> WorkerM ()) -> IO ()
start queue handler = do
    state <- loadState
    putStrLn "Running worker"
    print queue
    runReaderT (connect queue handler) state


runIO :: AppState -> WorkerM a -> IO a
runIO s x = do
  runReaderT x s



-- standardized error handling
onError :: MonadWorker m => WorkerException SomeException -> m ()
onError e = do
    liftIO $ putStrLn "Do something with errors: careful with PII in logs!"
    liftIO $ print e
    -- TODO handle errors. Create an error queue?
    -- TODO send to rollbar or somewhere similar

