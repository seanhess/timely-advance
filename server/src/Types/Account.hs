{-# LANGUAGE DeriveGeneric     #-}
module Types.Account where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Selda (SqlRow)
import Types.Id (Id)



data Account = Account
    { accountId :: Id Account
    , firstName :: Text
    , lastName :: Text
    , email :: Text
    , plaidToken :: Text
    } deriving (Generic, Show)

instance SqlRow Account
instance ToJSON Account
instance FromJSON Account

