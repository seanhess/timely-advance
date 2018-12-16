{-# LANGUAGE DeriveGeneric     #-}
module Types.AccountInfo where

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


