{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Timely.Worker.WorkerM where

import           Control.Exception         (SomeException)
import           Control.Monad.Config      (MonadConfig (..))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Log         (LogT, runLogT)
import qualified Control.Monad.Log         as Log
import           Control.Monad.Logger      (MonadLogger, logErrorN)
import           Control.Monad.Reader      (ReaderT, asks, runReaderT)
import           Control.Monad.Selda       (Selda (..))
import           Data.Aeson                (FromJSON)
import           Data.Pool                 (Pool)
import qualified Data.Pool                 as Pool
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text)
import           Database.Selda            (MonadMask)
import           Database.Selda.Backend    (SeldaConnection)
import qualified Database.Selda.PostgreSQL as Selda
import           Network.AMQP.Worker       (Queue (Queue), WorkerException, def)
import qualified Network.AMQP.Worker       as Worker hiding (bindQueue, publish, worker)
import           Network.AMQP.Worker.Monad (MonadWorker (..))
import qualified Network.AMQP.Worker.Monad as Worker
import qualified Network.Dwolla            as Dwolla
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Client.TLS   as HTTP
import qualified Network.Plaid             as Plaid

import qualified Timely.Bank               as Bank
import           Timely.Config             (Env (..), loadEnv)
import qualified Timely.Notify             as Notify

data AppState = AppState
  { dbConn   :: Pool SeldaConnection
  , amqpConn :: Worker.Connection
  , plaid    :: Plaid.Credentials
  , manager  :: HTTP.Manager
  , env      :: Env
  }

-- LogT must be on top since I didn't define M^2 instances for it
type WorkerM = ReaderT AppState IO
type HandlerM = LogT WorkerM

instance Selda HandlerM where
  withConnection action = do
    pool <- asks dbConn
    Pool.withResource pool action

instance Selda WorkerM where
  withConnection action = do
    pool <- asks dbConn
    Pool.withResource pool action

instance MonadWorker WorkerM where
  amqpConnection = asks amqpConn

instance MonadConfig Bank.Config HandlerM where
  config = do
    c <- asks plaid
    m <- asks manager
    b <- asks (plaidBaseUrl . env)
    pure $ Bank.Config { Bank.manager = m, Bank.baseUrl = b, Bank.credentials = c }

instance MonadConfig Notify.Config HandlerM where
  config = do
    e <- asks env
    pure $ Notify.Config (twilioFromPhone e) (twilioAccountId e) (twilioAuthToken e) (endpoint e)

instance MonadConfig Dwolla.Credentials HandlerM where
    config = do
      e <- asks env
      pure $ Dwolla.Credentials (dwollaClientId e) (dwollaSecret e)


loadState :: (MonadIO m, MonadMask m) => m AppState
loadState = do
  env <- loadEnv
  amqpConn <- Worker.connect (Worker.fromURI $ cs $ amqp env)
  dbConn <- liftIO $ Pool.createPool (createConn $ cs $ postgres env) destroyConn 1 5 3
  manager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  let plaid = Plaid.Credentials (plaidClientId env) (plaidClientSecret env)
  pure AppState {..}

  where createConn = Selda.pgOpen' Nothing
        destroyConn = Selda.seldaClose





connect :: forall a. (FromJSON a) => Queue a -> (a -> HandlerM ()) -> WorkerM ()
connect queue handler = do
  Worker.bindQueue queue
  Worker.worker def queue
    (\e -> runLogT $ onError (queueName queue) e)
    (\m -> runLogT $ onMessage m)

  where onMessage :: Worker.Message a -> HandlerM ()
        onMessage m = do
          Log.context (queueName queue)
          handler (Worker.value m)

        queueName queue = let (Queue _ name) = queue in name


start :: (FromJSON a) => Queue a -> (a -> HandlerM ()) -> IO ()
start queue handler = do
  state <- loadState
  putStrLn "Running worker"
  runReaderT (connect queue handler) state


runIO :: HandlerM a -> IO a
runIO x = do
  s <- loadState
  runReaderT (runLogT x) s








-- standardized error handling
-- FOR NOW: exceptions must include all context
onError :: (MonadLogger m) => Text -> WorkerException SomeException -> m ()
onError n e = do
  -- only used for AMQP-worker errors, we will catch everything ourselves!
  logErrorN $ n <> " " <> (cs $ show e)
