{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Accounts.Application
    ( fromAccountInfo
    , initialize
    , ApplicationStore(..)
    ) where

import Database.Selda
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Control.Monad.Effect (Effect(..))
import GHC.Generics (Generic)

import Types.Application (Application(..))
import Types.Account (Account)
import Types.Account.AccountInfo (AccountInfo(..))
import Types.Id (Id)


data ApplicationStore a where
    Save      :: Application -> ApplicationStore ()
    Find      :: Id Account  -> ApplicationStore (Maybe Application)
    All       :: ApplicationStore [Application]

instance (MonadSelda m) => Effect m ApplicationStore where
    run (Save a) = saveApplication a
    run (Find i) = findApplication i
    run All      = allApplications


fromAccountInfo :: Id Account -> AccountInfo -> Application
fromAccountInfo i AccountInfo {..} = Application {..}
  where
    accountId = i


applications :: Table Application
applications = table "accounts_applications" [#accountId :- primary]


saveApplication :: (MonadSelda m) => Application -> m ()
saveApplication app = do
    insert applications [app]
    pure ()


findApplication :: (MonadSelda m) => Id Account -> m (Maybe Application)
findApplication i = do
    as <- query $ do
      app <- select applications
      restrict (app ! #accountId .== literal i)
      return app
    pure $ listToMaybe as


allApplications :: (MonadSelda m) => m [Application]
allApplications =
    query $ select applications



initialize :: (MonadSelda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable applications
