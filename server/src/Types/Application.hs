{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Types.Application where

import Database.Selda
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)

import Types.Account (Account)
import Types.Account.AccountInfo (AccountInfo(..))
import Types.Id (Id)
import Types.Plaid
import Servant.API.ContentTypes.HTML (Linkable(..))



-- These database types belong to me

data Application = Application
    { accountId :: Id Account
    , firstName :: Text
    , lastName :: Text
    , email :: Text
    , plaidToken :: Token Access
    } deriving (Generic, Show)

instance FromJSON Application
instance ToJSON Application
instance SqlRow Application


instance Linkable Application where
    linkId = cs . UUID.toText . accountId

