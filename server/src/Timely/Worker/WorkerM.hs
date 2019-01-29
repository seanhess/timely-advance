{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Timely.Worker.WorkerM where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Config (MonadConfig(..))
import           Control.Monad.Except (runExceptT, ExceptT)
import           Control.Monad.Reader (ReaderT, runReaderT, asks)
import           Control.Exception (SomeException, throwIO, Exception)
import           Control.Monad.Selda (Selda(..))
import           Data.Pool (Pool)
import qualified Data.Pool as Pool
import           Data.Aeson (FromJSON)
import           Data.String.Conversions (cs)
import qualified Database.Selda.PostgreSQL as Selda
import           Database.Selda (MonadMask)
import           Database.Selda.Backend (SeldaConnection)
import           Network.AMQP.Worker (Queue, WorkerException, def)
import qualified Network.AMQP.Worker as Worker hiding (publish, bindQueue, worker)
import qualified Network.AMQP.Worker.Monad as Worker
import           Network.AMQP.Worker.Monad (MonadWorker(..))
import qualified Network.Plaid as Plaid
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import Timely.Config (loadEnv, Env(..))
import qualified Timely.Bank as Bank


data AppState = AppState
    { dbConn :: Pool SeldaConnection
    , amqpConn :: Worker.Connection
    , plaid :: Plaid.Credentials
    , manager :: HTTP.Manager
    , env :: Env
    }

type WorkerM = ReaderT AppState IO
type WorkerEM e = ExceptT e WorkerM

instance Selda (WorkerEM e) where
    withConnection action = do
      pool <- asks dbConn
      Pool.withResource pool action

instance MonadWorker (WorkerEM e) where
    amqpConnection = asks amqpConn

instance MonadWorker WorkerM where
    amqpConnection = asks amqpConn

instance MonadConfig Bank.Config (WorkerEM e) where
    config = do
      c <- asks plaid
      m <- asks manager
      b <- asks (plaidBaseUrl . env)
      pure $ Bank.Config { Bank.manager = m, Bank.baseUrl = b, Bank.credentials = c }


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


connect :: forall a e. (FromJSON a, Exception e) => Queue a -> (a -> WorkerEM e ()) -> WorkerM ()
connect queue handler = do
    Worker.bindQueue queue
    Worker.worker def queue onError onMessage

  where
    onMessage :: Worker.Message a -> WorkerM ()
    onMessage m = runWorkerEM $ handler (Worker.value m)



start :: (FromJSON a, Exception e) => Queue a -> (a -> WorkerEM e ()) -> IO ()
start queue handler = do
    state <- loadState
    putStrLn "Running worker"
    runReaderT (connect queue handler) state


runWorkerEM :: Exception e => WorkerEM e a -> WorkerM a
runWorkerEM x = do
    e <- runExceptT x
    case e of
      Left err -> liftIO $ throwIO err
      Right a -> pure a


runIO :: Exception e => AppState -> WorkerEM e a -> IO a
runIO s x = do
  runReaderT (runWorkerEM x) s



-- standardized error handling
onError :: MonadWorker m => WorkerException SomeException -> m ()
onError e = do
    liftIO $ putStrLn "Do something with errors: careful with PII in logs!"
    liftIO $ print e
    -- TODO handle errors. Create an error queue?
    -- TODO send to rollbar or somewhere similar

