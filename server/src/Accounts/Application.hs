{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Accounts.Application where

import Database.Selda
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

import Types.Application (Application(..))
import Types.AccountInfo (AccountInfo(..))
import Types.Account (Account)
import Types.Id (Id)


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
