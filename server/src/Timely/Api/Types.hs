{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Api.Types
  ( Account(..)
  , Customer(..)
  , BankAccount(..)
  , AccountInfo(..)
  , Amount(..)
  ) where

import Data.Aeson                    (FromJSON, ToJSON (..))
import Data.Model.Guid               as Guid (toText)
import Data.Model.Money              (Money)
import Data.String.Conversions       (cs)
import Data.Text                     (Text)
import GHC.Generics                  (Generic)
import Servant.API.ContentTypes.HTML (Linkable (..))
import Timely.Accounts.Types
import Timely.Bank                   (Public, Token)


data Amount = Amount
    { amount :: Money }
    deriving (Show, Eq, Generic)

instance FromJSON Amount


-- AccountInfo ---------------------
data AccountInfo = AccountInfo
    { publicBankToken :: Token Public
    , email           :: Text
    } deriving (Generic, Show)

instance FromJSON AccountInfo
instance ToJSON AccountInfo




instance Linkable Account where
    self a = cs $ Guid.toText $ accountId (a :: Account)
    relations _ = ["bank-accounts"]

instance Linkable Application where
    self a = cs $ Guid.toText $ accountId (a :: Application)
    relations _ = ["result"]

instance Linkable BankAccount where
    self _ = ""
