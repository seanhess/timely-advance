{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
module AccountStore.Application
    ( initialize
    , ApplicationStore(..)
    ) where

import Control.Monad.Selda (Selda, query, insert, tryCreateTable)
import Database.Selda hiding (query, insert, tryCreateTable)
import Data.Maybe (listToMaybe)
import Control.Monad.Service (Service(..))

import AccountStore.Types
import Types.Guid (Guid)


data ApplicationStore a where
    Save      :: Application -> ApplicationStore ()
    Find      :: Guid Account  -> ApplicationStore (Maybe Application)
    All       :: ApplicationStore [Application]

instance (Selda m) => Service m ApplicationStore where
    run (Save a) = saveApplication a
    run (Find i) = findApplication i
    run All      = allApplications



applications :: Table Application
applications = table "accounts_applications" [#accountId :- primary]


saveApplication :: (Selda m) => Application -> m ()
saveApplication app = do
    insert applications [app]
    pure ()


findApplication :: (Selda m) => Guid Account -> m (Maybe Application)
findApplication i = do
    as <- query $ do
      app <- select applications
      restrict (app ! #accountId .== literal i)
      return app
    pure $ listToMaybe as


allApplications :: (Selda m) => m [Application]
allApplications =
    query $ select applications



initialize :: (Selda m, MonadIO m) => m ()
initialize =
    -- drop the table / db first to run migrations
    tryCreateTable applications
