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


-- TODO separate customer
-- TODO create a state machine for account: bank status
-- TODO multiple banks


-- serialize this to json?
-- or, ... naw, use another table. so fun
-- besides, the customer info is what they would actually update, not the rest of this stuff
-- data Customer = Customer
--     { firstName :: Text
--     , lastName :: Text
--     , email :: Text
--     } deriving (Show, Eq, Generic)




data Account = Account
    { accountId :: Id Account
    , firstName :: Text
    , lastName :: Text
    , email :: Text
    , plaidToken :: Text
    , bankBalance :: Int
    } deriving (Generic, Eq, Show)

instance SqlRow Account
instance ToJSON Account
instance FromJSON Account



-- this isn't necessarily the ideal format for the database
-- let's hold off on all this stuff until I get the worker working

-- data Bank = Bank
--     { bankToken :: Text
--     , bankAccounts :: [BankAccount]
--     } deriving (Generic, Eq, Show)


-- data BankAccount = BankAccount
--     { accountId :: Id Account
--     , balance :: Int
--     } deriving (Generic, Eq, Show)
