{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
module Timely.Types.Result where

import           Control.Applicative    ((<|>))
import           Control.Effects.Signal (handleToEither)
import           Control.Monad.Except   (ExceptT)
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import           GHC.Generics           (Generic)

-- | An error optional type equivalent to Either, which serializes to either the error or the value. Error types should be unique, and probably contain an error field
data Result e a
  = Ok a
  | Err e
  deriving (Show, Eq, Generic)

instance (ToJSON a, ToJSON e) => ToJSON (Result e a) where
  toJSON (Ok a)  = toJSON a
  toJSON (Err e) = toJSON e

instance (FromJSON a, FromJSON e) => FromJSON (Result e a) where
  parseJSON v =
    (Ok <$> parseJSON v) <|> (Err <$> parseJSON v)



fromEither :: Either e a -> Result e a
fromEither (Left e)  = Err e
fromEither (Right a) = Ok a


handle :: forall e a m. Functor m => ExceptT e m a -> m (Result e a)
handle x = do
 fromEither <$> handleToEither x
