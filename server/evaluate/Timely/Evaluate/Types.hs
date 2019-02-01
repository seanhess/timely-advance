{-# LANGUAGE DeriveGeneric #-}
module Timely.Evaluate.Types where

import           Data.Aeson         (FromJSON, ToJSON)
import           GHC.Generics       (Generic)
import           Timely.Types.Money

data Projection = Projection
    { expenses  :: Money
    , available :: Money
    } deriving (Show, Eq, Generic)

instance ToJSON Projection
instance FromJSON Projection
