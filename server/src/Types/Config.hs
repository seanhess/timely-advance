{-# LANGUAGE DeriveGeneric #-}
module Types.Config where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)

data ClientConfig = ClientConfig
    { plaid :: PlaidConfig
    } deriving (Generic, Show, Eq)

instance ToJSON ClientConfig

data PlaidConfig = PlaidConfig
    { publicKey :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON PlaidConfig

