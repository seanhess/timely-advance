{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Timely.Api.Applications
    ( newApplication
    ) where

import           Control.Monad.Service (Service(..))
import           Control.Monad.Config (MonadConfig(..))
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Network.AMQP.Worker.Monad (MonadWorker)
import qualified Network.AMQP.Worker.Monad as Worker
import           Servant (ServantErr)
import           Servant.Auth.Server (CookieSettings, JWTSettings)

import           Timely.AccountStore.Types (Application(..), Account)
import           Timely.Api.Types (AccountInfo(..))
import           Timely.Api.Sessions (SetSession)
import qualified Timely.Api.Sessions as Sessions
import qualified Timely.AccountStore.Application as Application
import           Timely.AccountStore.Application   (ApplicationStore)

import qualified Timely.Events as Events

import           Timely.Auth (Phone)
import           Timely.Types.Guid (Guid, randomId)
import           Timely.Types.Session (Session(..))


newApplication
  :: ( MonadWorker m
     , Service m ApplicationStore
     , MonadIO m
     , MonadError ServantErr m
     , MonadConfig CookieSettings m
     , MonadConfig JWTSettings m
     ) => Phone -> AccountInfo -> m (SetSession Application)
newApplication phone info = do
    -- create an application
    accountId <- randomId
    let app = fromAccountInfo accountId phone info

    -- save it
    run $ Application.Save app

    -- publish it
    liftIO $ putStrLn "PUBLISHING"
    liftIO $ print app
    Worker.publish Events.applicationsNew app

    -- set the session
    Sessions.setSession (Session phone (Just accountId)) app



fromAccountInfo :: Guid Account -> Phone -> AccountInfo -> Application
fromAccountInfo i phone AccountInfo {..} = Application {..}
  where
    accountId = i
