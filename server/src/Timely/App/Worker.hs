{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
module Timely.App.Worker where

import           Control.Effects                 (MonadEffect (..))
import           Control.Effects.Log             (Log (..))
import qualified Control.Effects.Log             as Log
import           Control.Exception               (SomeException)
import           Data.Aeson                      (FromJSON)
import           Data.String.Conversions         (cs)
import           Data.Text                       (Text)
import           Network.AMQP.Worker             (Queue (Queue), WorkerException, def)
-- import qualified Network.AMQP.Worker             as Worker hiding (bindQueue, publish, worker)
import qualified Network.AMQP.Worker             as Worker
import           Timely.App.AppM                 (AppT, loadState, runApp, AppState(amqpConn))




start :: forall a. (FromJSON a) => Queue a -> (a -> AppT IO ()) -> IO ()
start queue handler = do
  state <- loadState
  let conn = amqpConn state
  putStrLn $ "Worker: " ++ cs (queueName queue)

  -- it needs the connection, forget the stupid MonadWorker
  Worker.bindQueue conn queue
  Worker.worker conn def queue
    (\e -> runApp state $ onError (queueName queue) e)
    (\m -> runApp state $ onMessage m)

  where
    queueName queue = let (Queue _ name) = queue in name

    onMessage :: Worker.Message a -> AppT IO ()
    onMessage m = do
      Log.context (queueName queue)
      handler (Worker.value m)


    -- standardized error handling
    -- FOR NOW: exceptions must include all context
    onError :: (MonadEffect Log m) => Text -> WorkerException SomeException -> m ()
    onError n e = do
      -- only used for AMQP-worker errors, we will catch everything ourselves!
      Log.error $ n <> " " <> (cs $ show e)
