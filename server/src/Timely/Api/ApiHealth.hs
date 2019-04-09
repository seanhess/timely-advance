{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Api.ApiHealth where


import           Control.Effects             (MonadEffects)
import           Control.Effects.Worker      (Publish)
import qualified Control.Effects.Worker      as Worker
import           Data.Text                   (Text, pack)
import           Timely.Accounts.Application as Application
import           Timely.Events               as Events
import           Version



health
  :: ( MonadEffects [Publish, Applications] m)
  => m Text
health = do
    _ <- Application.check
    Worker.publish Events.health "OK"
    pure $ "Timely " <> pack Version.version
