{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Api.Applications
    ( newApplication
    ) where

import           Control.Monad.Config            (MonadConfig (..))
import           Control.Monad.Except            (MonadError (..))
import           Control.Monad.Log               as Log
import           Control.Monad.Service           (Service (..))
import           Data.Model.Guid                 as Guid
import           Data.String.Conversions         (cs)
import           Network.AMQP.Worker.Monad       as Worker
import           Servant                         (ServantErr)
import           Servant.Auth.Server             (CookieSettings, JWTSettings)
import           Timely.AccountStore.Application as Application
import           Timely.AccountStore.Types       (Account, Application (..), Phone)
import           Timely.Api.Sessions             as Sessions
import           Timely.Api.Types                (AccountInfo (..))
import           Timely.Events                   as Events
import           Timely.Types.Session            (Session (..))


newApplication
  :: ( MonadWorker m
     , Service m ApplicationStore
     , MonadError ServantErr m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     , MonadLog m
     ) => Phone -> AccountInfo -> m (SetSession Application)
newApplication phone info = do
    -- create an application
    accountId <- Guid.randomId
    Log.context (cs $ show phone)
    Log.context (Guid.toText accountId)
    Log.info "new application"
    let app = fromAccountInfo accountId phone info
    Log.debug ("application", app)

    -- save it
    run $ Application.Save app

    -- publish it
    Worker.publish Events.applicationsNew app
    Log.info "published"

    -- set the session
    Sessions.setSession (Session phone (Just accountId) False) app



fromAccountInfo :: Guid Account -> Phone -> AccountInfo -> Application
fromAccountInfo i phone AccountInfo { email, ssn, dateOfBirth, publicBankToken } =
  Application
    { accountId = i, phone, email, ssn, dateOfBirth, publicBankToken }
