{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Api.Applications
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

import AccountStore.Types (Application(..), Account)
import Auth (Phone)
import Api.Types (AccountInfo(..))
import           Api.Sessions (SetSession)
import qualified Api.Sessions as Sessions
import qualified AccountStore.Application as Application
import           AccountStore.Application   (ApplicationStore)
import qualified Events
import Types.Guid (Guid, randomId)
import Types.Session (Session(..))


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
