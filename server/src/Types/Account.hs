{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
module Types.Account
    ( Account(..)
    , AccountId
    ) where

import Data.Function ((&))
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Selda (SqlRow)
import Types.Id (Id)
import Types.Account.AccountId
import Types.Account.Customer (Customer)
import Types.Account.Bank (Bank)



data Account = Account
    { accountId :: Id AccountId
    , bank :: Bank
    , customer :: Customer
    } deriving (Generic, Eq, Show)

-- you can't save this directly
instance ToJSON Account
instance FromJSON Account


