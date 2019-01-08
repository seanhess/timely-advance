{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Types.Account.Bank where


import Database.Selda
import Data.Aeson (ToJSON, FromJSON)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Types.Id (Id)
import Types.Account.AccountId (AccountId)
import Types.Plaid
import Network.Plaid.Types (Token(..), Access)

data Bank = Bank
    { accountId :: Id AccountId
    , balance :: Int

    -- TODO switch to plaid key
    , accessToken :: Token Access
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
