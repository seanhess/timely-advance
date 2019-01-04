{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
module Api.Applications
    ( newApplication
    ) where

import qualified Events
import qualified Accounts.Application as App
import Control.Monad.Effect (Effect(..))
import Control.Monad.IO.Class (liftIO)
import Database.Selda (MonadSelda)
import Types.Id (Id(..), randomId)
import Network.AMQP.Worker.Monad (MonadWorker)
import qualified Network.AMQP.Worker.Monad as Worker
import Types.Application (Application)

import Types.Account.AccountInfo (AccountInfo(..))


-- TODO switch to Effect m ApplicationStore constraint
newApplication :: (MonadWorker m, MonadSelda m) => AccountInfo -> m Application
newApplication info = do
    -- create an application
    accountId <- randomId
    let app = App.fromAccountInfo accountId info

    -- save it
    run $ App.Save app

    -- publish it
    liftIO $ putStrLn $ "PUBLISHING"
    liftIO $ print $ app
    Worker.publish Events.applicationsNew app

    pure app


