{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Api.Applications
    ( newApplication
    ) where

import           Control.Effects                 (MonadEffects)
import           Control.Effects.Log             as Log
import           Control.Effects.Signal          (Throw)
import           Control.Effects.Time            (Time, UTCTime)
import qualified Control.Effects.Time            as Time
import           Control.Effects.Worker          (Publish)
import qualified Control.Effects.Worker          as Worker
import           Control.Monad.Config            (MonadConfig (..))
import           Control.Monad.IO.Class          (MonadIO)
import           Data.Model.Guid                 as Guid
import           Data.Model.Types                (Phone, Valid)
import           Data.String.Conversions         (cs)
import           Servant                         (ServantErr)
import           Servant.Auth.Server             (CookieSettings, JWTSettings)
import           Timely.AccountStore.Application as Application
import           Timely.AccountStore.Types       (Account, Application (..))
import           Timely.Api.Sessions             as Sessions
import           Timely.Api.Types                (AccountInfo (..))
import           Timely.Events                   as Events
import           Timely.Types.Session            (Session (..))



newApplication
  :: ( MonadIO m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     , MonadEffects '[Log, Throw ServantErr, Time, Publish, Applications] m
     )
  => Valid Phone -> AccountInfo -> m (SetSession Application)
newApplication phone info = do
    accountId <- Guid.randomId
    app <- createNewApplication phone info accountId
    Sessions.setSession (Session phone (Just accountId) False) app



createNewApplication
  :: ( MonadEffects '[Log, Time, Publish, Applications] m)
  => Valid Phone -> AccountInfo -> Guid Account -> m Application
createNewApplication phone info accountId = do
    Log.context (cs $ show phone)
    Log.context (Guid.toText accountId)
    Log.info "new application"

    now <- Time.currentTime
    let app = fromAccountInfo accountId now phone info
    Log.debug ("application", app)

    -- save it
    Application.save app

    -- publish it
    Worker.publish Events.applicationsNew app
    Log.info "published"
    pure app






fromAccountInfo :: Guid Account -> UTCTime -> Valid Phone -> AccountInfo -> Application
fromAccountInfo i now phone AccountInfo { email, ssn, dateOfBirth, publicBankToken } =
  Application
    { accountId = i, phone, email, ssn, dateOfBirth, publicBankToken, created = now }
