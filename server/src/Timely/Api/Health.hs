{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Api.Health where


import           Control.Monad.Service           (Service (..))
import           Data.Text                       (Text, pack)
-- import qualified Network.AMQP.Worker.Monad       as Worker
-- import Network.AMQP.Worker.Monad       (MonadWorker)
import           Control.Effects                 (MonadEffect)
import           Control.Effects.Worker          (Publish)
import           Timely.AccountStore.Application as Application
import qualified Control.Effects.Worker           as Worker
import           Timely.Events                   as Events
import           Version



health
  :: ( MonadEffect Publish m
     , Service m ApplicationStore
     ) => m Text
health = do
    _ <- run $ Application.Check
    Worker.publish Events.health "OK"
    pure $ "Timely " <> pack Version.version
