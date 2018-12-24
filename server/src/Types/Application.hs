{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Types.Application where

import Database.Selda
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

import Types.Account (Account)
import Types.Account.AccountInfo (AccountInfo(..))
import Types.Id (Id)



-- These database types belong to me

data Application = Application
    { accountId :: Id Account
    , firstName :: Text
    , lastName :: Text
    , email :: Text
    , plaidToken :: Text
    } deriving (Generic, Show)

instance FromJSON Application
instance ToJSON Application
instance SqlRow Application
