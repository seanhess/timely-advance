{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Timely.Worker.WorkerM where

import           Control.Effects           (MonadEffect (..), RuntimeImplemented (..))
import           Control.Effects.Log       (Log (..), implementLogStdout)
import qualified Control.Effects.Log       as Log
import           Control.Effects.Time      (Time, implementTimeIO)
import           Control.Effects.Worker    (Publish (..), implementAMQP)
import           Control.Exception         (SomeException)
import           Control.Monad.Config      (MonadConfig (..))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (ReaderT, asks, runReaderT)
import           Control.Monad.Selda       (Selda (..))
import           Data.Aeson                (FromJSON)
import           Data.Function             ((&))
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
import           Timely.App                (retry)
import qualified Timely.Bank               as Bank
import           Timely.Config             (Env (..), loadEnv)
import qualified Timely.Notify             as Notify
import qualified Timely.Transfers          as Transfers

data AppState = AppState
  { dbConn   :: Pool SeldaConnection
  , amqpConn :: Worker.Connection
  , plaid    :: Plaid.Credentials
  , manager  :: HTTP.Manager
  , env      :: Env
  }


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

instance MonadWorker HandlerM where
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
    pure $ Notify.Config (twilioFromPhone e) (twilioAccountId e) (twilioAuthToken e) (appEndpoint e)

instance MonadConfig Dwolla.Credentials HandlerM where
    config = do
      e <- asks env
      pure $ Dwolla.Credentials (dwollaClientId e) (dwollaSecret e)

instance MonadConfig Transfers.Config HandlerM where
    config = do
      dwolla <- config
      mgr <- asks manager
      base <- asks (dwollaBaseUrl . env)
      auth <- asks (dwollaAuthBaseUrl . env)
      src <- asks (dwollaFundingSource . env)
      pure $ Transfers.Config src (Dwolla.Config mgr base auth dwolla)


loadState :: (MonadIO m, MonadMask m) => m AppState
loadState = do
  env <- loadEnv
  amqpConn <- retry $ Worker.connect (Worker.fromURI $ cs $ amqp env)
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
    (\e -> runHandlerM $ onError (queueName queue) e)
    (\m -> runHandlerM $ onMessage m)

  where onMessage :: Worker.Message a -> HandlerM ()
        onMessage m = do
          Log.context (queueName queue)
          handler (Worker.value m)

        queueName queue = let (Queue _ name) = queue in name


start :: (FromJSON a) => Queue a -> (a -> HandlerM ()) -> IO ()
start queue handler = do
  state <- loadState
  putStrLn $ "Worker: " ++ cs (queueName queue)
  runWorkerM (connect queue handler) state
  where
    queueName queue = let (Queue _ name) = queue in name



-- Replace HandlerM and WorkerM with _ to have ghc suggest types for WorkerM and HandlerM
type WorkerM = ReaderT AppState (RuntimeImplemented Time (RuntimeImplemented Publish IO))
type HandlerM = RuntimeImplemented Log (Log.LogT WorkerM)


runWorkerM :: WorkerM a -> AppState -> IO a
runWorkerM x s =
  runReaderT x s
       & implementTimeIO
       & implementAMQP (amqpConn s)


runHandlerM :: HandlerM a -> WorkerM a
runHandlerM x = x & implementLogStdout



runIO :: HandlerM a -> IO a
runIO x = do
  s <- loadState
  runWorkerM (runHandlerM x) s








-- standardized error handling
-- FOR NOW: exceptions must include all context
onError :: (MonadEffect Log m) => Text -> WorkerException SomeException -> m ()
onError n e = do
  -- only used for AMQP-worker errors, we will catch everything ourselves!
  Log.error $ n <> " " <> (cs $ show e)
