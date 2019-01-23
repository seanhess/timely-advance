{-# LANGUAGE DeriveGeneric #-}
module Timely.Types.Config where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Network.Plaid.Types (Id, Public)

data ClientConfig = ClientConfig
    { plaid :: PlaidConfig
    } deriving (Generic, Show, Eq)

instance ToJSON ClientConfig

data PlaidConfig = PlaidConfig
    { publicKey :: Id Public
    } deriving (Generic, Show, Eq)

instance ToJSON PlaidConfig

