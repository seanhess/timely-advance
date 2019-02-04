{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Api.Types
  ( Account(..)
  , Customer(..)
  , BankAccount(..)
  , AccountInfo(..)
  , Result(..)
  ) where

import           Data.Aeson                    (FromJSON, ToJSON (..))
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text)
import qualified Data.UUID                     as UUID
import           Database.Selda                (ID, fromId)
import           GHC.Generics                  (Generic)
import           Servant.API.ContentTypes.HTML (Linkable (..))

import           Timely.AccountStore.Types
import           Timely.Advances               (Advance)
import           Timely.Bank                   (Public, Token)
import           Timely.Underwriting           (Approval, Denial, DenialReason,
                                                Result (..))


instance ToJSON Result where
    toJSON (Approved a) = toJSON a
    toJSON (Denied d)   = toJSON d

instance ToJSON Approval
instance ToJSON Denial
instance ToJSON DenialReason


instance ToJSON (ID a) where
    toJSON i = toJSON $ fromId i
instance ToJSON Health
instance ToJSON Account
instance ToJSON Customer
instance ToJSON BankAccountType
instance ToJSON BankAccount
instance ToJSON Application
instance ToJSON Advance
instance FromJSON Application


-- AccountInfo ---------------------
data AccountInfo = AccountInfo
    { email           :: Text
    , publicBankToken :: Token Public
    } deriving (Generic, Show)

instance FromJSON AccountInfo
instance ToJSON AccountInfo




instance Linkable Account where
    self a = cs $ UUID.toText $ accountId (a :: Account)
    relations _ = ["bank-accounts"]

instance Linkable Application where
    self a = cs $ UUID.toText $ accountId (a :: Application)
    relations _ = ["result"]

instance Linkable BankAccount where
    self _ = ""
