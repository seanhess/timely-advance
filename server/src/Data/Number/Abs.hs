{-# LANGUAGE DeriveGeneric #-}
module Data.Number.Abs where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)


newtype Abs a = Abs
  { value :: a }
  deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (Abs a)
instance FromJSON a => FromJSON (Abs a)


absolute :: Num a => a -> Abs a
absolute a = Abs (abs a)
