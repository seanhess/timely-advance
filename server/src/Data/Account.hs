{-# LANGUAGE DeriveGeneric     #-}
module Data.Account where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data AccountInfo = AccountInfo
    { firstName :: Text
    , lastName :: Text
    , email :: Text
    } deriving (Generic, Show)

instance FromJSON AccountInfo
instance ToJSON AccountInfo


data Account = Account
    { accountId   :: Text
    , accountInfo :: AccountInfo
    } deriving (Generic, Show)

instance ToJSON Account
instance FromJSON Account
