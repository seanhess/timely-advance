{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Timely.Types.Config where

import           Data.Aeson          (ToJSON)
import           Data.Model.Id       (Id)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Network.Plaid.Types (Public)

data ClientConfig = ClientConfig
    { version :: String
    , plaid :: PlaidConfig
    } deriving (Generic, Show, Eq)

instance ToJSON ClientConfig



newtype PlaidProducts = PlaidProducts [Text]
  deriving (Show, Eq, Generic, ToJSON)


data PlaidConfig = PlaidConfig
    { publicKey :: Id Public
    , products  :: PlaidProducts
    , env       :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON PlaidConfig
