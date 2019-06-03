{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Experian.Address where

import Data.Aeson   (ToJSON)
import Data.Text    (Text)
import GHC.Generics (Generic)

data Address = Address
  { line1   :: Text
  , line2   :: Maybe Text
  , city    :: Text
  , state   :: State
  , zipCode :: ZipCode
  } deriving (Show, Generic)
instance ToJSON Address


newtype State = State Text
  deriving (Show, Generic, ToJSON)


newtype ZipCode = ZipCode Text
  deriving (Show, Generic, ToJSON)
