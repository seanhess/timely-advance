{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Types.Account.Bank where


import Database.Selda
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Database.Selda (SqlRow)
import GHC.Generics (Generic)
import Types.Id (Id)
import Types.Account.AccountId (AccountId)

data Bank = Bank
    { accountId :: Id AccountId
    , balance :: Int
    -- TODO switch to plaid key
    , accessToken :: Text
    } deriving (Generic, Eq, Show)

instance SqlRow Bank
instance ToJSON Bank
instance FromJSON Bank




-- data Bank = Bank
--     { bankToken :: Text
--     , bankAccounts :: [BankAccount]
--     } deriving (Generic, Eq, Show)


-- data BankAccount = BankAccount
--     { accountId :: Id Account
--     , balance :: Int
--     } deriving (Generic, Eq, Show)
