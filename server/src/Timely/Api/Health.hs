{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds  #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Api.Health where


import           Control.Monad.Service           (Service (..))
import           Data.Text                       (Text, pack)
import           Network.AMQP.Worker.Monad       as Worker
import           Timely.AccountStore.Application as Application
import           Timely.Events                   as Events
import           Version



health
  :: ( MonadWorker m
     , Service m ApplicationStore
     ) => m Text
health = do
    _ <- run $ Application.Check
    Worker.publish Events.health "OK"
    pure $ "Timely " <> pack Version.version
