{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DuplicateRecordFields     #-}
module Types.Account
  ( Account(..)
  , Customer(..)
  , BankAccount(..)
  , AccountInfo(..)
  ) where

import AccountStore.Types
import qualified AccountStore.Types as Accounts
import Data.Function ((&))
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.UUID as UUID
import Data.String.Conversions (cs)
import GHC.Generics (Generic)
import Database.Selda (SqlRow)
import Types.Guid (Guid)
import Types.Plaid
import Servant.API.ContentTypes.HTML (Linkable(..))


instance Linkable Account where
    self a = cs $ UUID.toText $ accountId (a :: Account)
    relations _ = ["bank-accounts"]

instance Linkable Application where
    self a = cs $ UUID.toText $ accountId (a :: Application)

instance Linkable BankAccount where
    self _ = ""


-- AccountInfo ---------------------
data AccountInfo = AccountInfo
    { firstName :: Text
    , lastName :: Text
    , email :: Text
    , publicBankToken :: Token Public
    } deriving (Generic, Show)

instance FromJSON AccountInfo
instance ToJSON AccountInfo




