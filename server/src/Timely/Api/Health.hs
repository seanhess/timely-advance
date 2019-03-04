{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds  #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Api.Health where


import           Control.Monad.Service           (Service (..))
import           Data.Text                       (Text, pack)
-- import qualified Network.AMQP.Worker.Monad       as Worker
-- import Network.AMQP.Worker.Monad       (MonadWorker)
import           Timely.AccountStore.Application as Application
import           Timely.Events                   as Events
import           Version
import qualified Timely.Effects.Worker as Worker
import Timely.Effects.Worker (Publish)
import Control.Effects (MonadEffect)



health
  :: ( MonadEffect Publish m
     , Service m ApplicationStore
     ) => m Text
health = do
    _ <- run $ Application.Check
    Worker.publish Events.health "OK"
    pure $ "Timely " <> pack Version.version
