{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DuplicateRecordFields     #-}
module Api.Types
  ( Account(..)
  , Customer(..)
  , BankAccount(..)
  , AccountInfo(..)
  ) where

import AccountStore.Types
import Bank (Token, Public)
import Data.Aeson (ToJSON(..), FromJSON)
import Data.Text (Text)
import qualified Data.UUID as UUID
import Data.String.Conversions (cs)
import Database.Selda (ID, fromId)
import GHC.Generics (Generic)
import Servant.API.ContentTypes.HTML (Linkable(..))



instance ToJSON (ID a) where
    toJSON i = toJSON $ fromId i
instance ToJSON Account
instance ToJSON Customer
instance ToJSON BankAccountType
instance ToJSON Balance
instance ToJSON BankAccount
instance ToJSON Application
instance FromJSON Application


-- AccountInfo ---------------------
data AccountInfo = AccountInfo
    { firstName :: Text
    , lastName :: Text
    , email :: Text
    , publicBankToken :: Token Public
    } deriving (Generic, Show)

instance FromJSON AccountInfo
instance ToJSON AccountInfo




instance Linkable Account where
    self a = cs $ UUID.toText $ accountId (a :: Account)
    relations _ = ["bank-accounts"]

instance Linkable Application where
    self a = cs $ UUID.toText $ accountId (a :: Application)

instance Linkable BankAccount where
    self _ = ""
