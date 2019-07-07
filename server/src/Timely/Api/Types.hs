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

import           Data.Aeson                    (FromJSON, ToJSON (..))
import qualified Data.Model.Guid               as Guid
import           Data.Model.Money              (Money)
import           Data.Model.Types              (SSN)
import           Data.Model.Valid              (Valid)
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text)
import           Data.Time.Calendar            (Day)
import           GHC.Generics                  (Generic)
import           Servant.API.ContentTypes.HTML (Linkable (..))
import           Timely.Accounts.Types
import           Timely.Bank                   (Public, Token)


data Amount = Amount
    { amount :: Money }
    deriving (Show, Eq, Generic)

instance FromJSON Amount


-- AccountInfo ---------------------
data AccountInfo = AccountInfo
    { email           :: Text
    , ssn             :: Valid SSN
    , dateOfBirth     :: Day
    , publicBankToken :: Token Public
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
