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
import           Control.Monad.Logger      (LoggingT, logErrorNS, logInfoNS, runStdoutLoggingT, MonadLogger)
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

type WorkerM = ReaderT AppState (LoggingT IO)

instance Selda WorkerM where
  withConnection action = do
    pool <- asks dbConn
    Pool.withResource pool action

instance MonadWorker WorkerM where
  amqpConnection = asks amqpConn

instance MonadConfig Bank.Config WorkerM where
  config = do
    c <- asks plaid
    m <- asks manager
    b <- asks (plaidBaseUrl . env)
    pure $ Bank.Config { Bank.manager = m, Bank.baseUrl = b, Bank.credentials = c }

instance MonadConfig Notify.Config WorkerM where
  config = do
    e <- asks env
    pure $ Notify.Config (twilioFromPhone e) (twilioAccountId e) (twilioAuthToken e) (endpoint e)


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


connect :: forall a. (FromJSON a) => Queue a -> (a -> WorkerM ()) -> WorkerM ()
connect queue handler = do
  Worker.bindQueue queue
  Worker.worker def queue (onError $ queueName queue) onMessage

  where onMessage :: Worker.Message a -> WorkerM ()
        onMessage m = do
          logInfoNS (queueName queue) "New Message"
          handler (Worker.value m)

        queueName queue = let (Queue _ name) = queue in name

start :: (FromJSON a) => Queue a -> (a -> WorkerM ()) -> IO ()
start queue handler = do
  state <- loadState
  putStrLn "Running worker"
  runStdoutLoggingT (runReaderT (connect queue handler) state)


-- runWorkerEM :: Exception e => WorkerEM e a -> WorkerM a
-- runWorkerEM x = do
--   e <- runExceptT x
--   case e of
--     Left err -> liftIO $ throwIO err
--     Right a  -> pure a


runIO :: WorkerM a -> IO a
runIO x = do
  s <- loadState
  runStdoutLoggingT (runReaderT x s)








-- standardized error handling
onError :: (MonadLogger m) => Text -> WorkerException SomeException -> m ()
onError n e = do
    -- TODO send to rollbar or somewhere similar
    -- TODO handle PII!
    logErrorNS n (cs $ show e)
