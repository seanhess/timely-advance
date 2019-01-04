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
import qualified Data.UUID as UUID
import Data.String.Conversions (cs)
import GHC.Generics (Generic)
import Database.Selda (SqlRow)
import Types.Id (Id)
import Types.Account.AccountId
import Types.Account.Customer (Customer)
import Types.Account.Bank (Bank)
import Servant.API.ContentTypes.HTML (Linkable(..))



data Account = Account
    { accountId :: Id AccountId
    , bank :: Bank
    , customer :: Customer
    } deriving (Generic, Eq, Show)

-- you can't save this directly
instance ToJSON Account
instance FromJSON Account


instance Linkable Account where
    linkId = cs . UUID.toText . accountId

