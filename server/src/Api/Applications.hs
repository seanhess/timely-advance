{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Api.Applications
    ( newApplication
    ) where

import qualified AccountStore.Application as Application
import           AccountStore.Application   (ApplicationStore)
import qualified Events
import Control.Monad.Service (Service(..))
import Control.Monad.IO.Class (liftIO)
import Types.Guid (Guid, randomId)
import Network.AMQP.Worker.Monad (MonadWorker)
import qualified Network.AMQP.Worker.Monad as Worker
import AccountStore.Types (Application(..), Account)

import Api.Types (AccountInfo(..))


-- TODO switch to Service m ApplicationStore constraint
newApplication :: (MonadWorker m, Service m ApplicationStore) => AccountInfo -> m Application
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
