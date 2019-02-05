{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Timely.Types.Secret where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

newtype Secret a = Secret Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
