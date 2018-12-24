{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Types.Account.Customer where


import Database.Selda
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Database.Selda (SqlRow)
import GHC.Generics (Generic)
import Types.Id (Id)
import Types.Account.AccountId (AccountId)

data Customer = Customer
    { accountId :: Id AccountId
    , firstName :: Text
    , lastName :: Text
    , email :: Text
    } deriving (Generic, Eq, Show)

instance SqlRow Customer
instance ToJSON Customer
instance FromJSON Customer


