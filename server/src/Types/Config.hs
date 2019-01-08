{-# LANGUAGE DeriveGeneric #-}
module Types.Config where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Network.Plaid.Types (Id, Public)

data ClientConfig = ClientConfig
    { plaid :: PlaidConfig
    } deriving (Generic, Show, Eq)

instance ToJSON ClientConfig

data PlaidConfig = PlaidConfig
    { publicKey :: Id Public
    } deriving (Generic, Show, Eq)

instance ToJSON PlaidConfig

