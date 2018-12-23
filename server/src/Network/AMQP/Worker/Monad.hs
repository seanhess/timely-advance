{-# LANGUAGE FlexibleContexts #-}
module Network.AMQP.Worker.Monad where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (liftBase)
import Data.Aeson (ToJSON)
import qualified Network.AMQP.Worker as Worker

class (MonadBaseControl IO m) => MonadWorker m where
    amqpConnection :: m Worker.Connection
    -- publish :: (ToJSON msg, MonadBaseControl IO m) => Queue Direct msg -> msg -> m ()


publish :: (MonadWorker m, ToJSON msg) => Worker.Queue Worker.Direct msg -> msg -> m ()
publish queue msg = do
  conn <- amqpConnection
  Worker.publish conn queue msg


initQueue :: (MonadWorker m) => Worker.Queue Worker.Direct msg -> m ()
initQueue queue = do
  conn <- amqpConnection
  liftBase $ Worker.initQueue conn queue
