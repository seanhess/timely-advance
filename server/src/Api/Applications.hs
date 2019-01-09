{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Api.Applications
    ( newApplication
    ) where

import qualified AccountStore.Application as Application
import qualified Events
import Control.Monad.Effect (Effect(..))
import Control.Monad.IO.Class (liftIO)
import Database.Selda (MonadSelda)
import Types.Guid (Guid(..), randomId)
import Network.AMQP.Worker.Monad (MonadWorker)
import qualified Network.AMQP.Worker.Monad as Worker
import AccountStore.Types (Application(..), Account)

import Types.Account (AccountInfo(..))


-- TODO switch to Effect m ApplicationStore constraint
newApplication :: (MonadWorker m, MonadSelda m) => AccountInfo -> m Application
newApplication info = do
    -- create an application
    accountId <- randomId
    let app = fromAccountInfo accountId info

    -- save it
    run $ Application.Save app

    -- publish it
    liftIO $ putStrLn "PUBLISHING"
    liftIO $ print app
    Worker.publish Events.applicationsNew app

    pure app



fromAccountInfo :: Guid Account -> AccountInfo -> Application
fromAccountInfo i AccountInfo {..} = Application {..}
  where
    accountId = i
