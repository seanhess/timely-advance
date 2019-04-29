{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Model.Meta where

import           Control.Applicative ((<|>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import qualified Data.HashMap.Strict as HM
import           Data.Proxy          (Proxy (..))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           GHC.TypeLits        (KnownSymbol, Symbol, symbolVal)

-- | A product type that can attach metadata under the field specified
data Meta (s :: Symbol) m a = Meta { meta :: m, value :: a }
  deriving (Show, Eq)


instance (KnownSymbol s, ToJSON m, ToJSON a) => ToJSON (Meta s m a) where
  toJSON (Meta m a) =
    case (toJSON m, toJSON a) of
      (Object mh, Object ah) -> Object $ HM.union mh ah
      (mv       , Object ah) -> Object $ HM.insert (fieldName (Proxy :: Proxy s)) mv ah
      (mv, ah)               -> error "Value is not a record"

instance (KnownSymbol s, FromJSON m, FromJSON a) => FromJSON (Meta s m a) where
  parseJSON v = flip (withObject "Meta") v $ \o -> do
    m <- parseJSON v <|> o .: fieldName (Proxy :: Proxy s)
    a <- parseJSON v
    pure $ Meta m a


instance Functor (Meta s m) where
  fmap f (Meta m a) = Meta m (f a)


fieldName :: KnownSymbol s => Proxy s -> Text
fieldName = Text.pack . symbolVal

