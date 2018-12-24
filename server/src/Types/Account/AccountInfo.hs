{-# LANGUAGE DeriveGeneric     #-}
module Types.Account.AccountInfo where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data AccountInfo = AccountInfo
    { firstName :: Text
    , lastName :: Text
    , email :: Text
    , plaidToken :: Text
    } deriving (Generic, Show)

instance FromJSON AccountInfo
instance ToJSON AccountInfo


