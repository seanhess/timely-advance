module Data.Number.Abs where

import           Data.Aeson (FromJSON (..), ToJSON (..))

newtype Abs a = Abs
  { value :: a }
  deriving (Show, Eq)

instance ToJSON a => ToJSON (Abs a) where
  toJSON (Abs a) = toJSON a

instance FromJSON a => FromJSON (Abs a) where
  parseJSON val = Abs <$> parseJSON val


absolute :: Num a => a -> Abs a
absolute a = Abs (abs a)

