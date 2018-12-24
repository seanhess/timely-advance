{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
module Types.Account where

import Data.Function ((&))
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Selda (SqlRow)
import Types.Id (Id)
import Types.AccountId (AccountId)
import Types.Customer (Customer)
import Types.Bank (Bank)



data Account = Account
    { accountId :: Id AccountId
    , bank :: Bank
    , customer :: Customer
    } deriving (Generic, Eq, Show)

-- you can't save this directly
instance ToJSON Account
instance FromJSON Account


