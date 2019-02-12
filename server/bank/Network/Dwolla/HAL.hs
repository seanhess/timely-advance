{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Network.Dwolla.HAL where


import           Servant
import           Data.Aeson               as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Servant.API.ContentTypes as Servant
import           Network.HTTP.Media       ((//))
import           Data.List.NonEmpty       (NonEmpty ((:|)))


data HAL


class FromHAL a where
  fromHAL :: ByteString -> Either String a
  default fromHAL :: FromJSON a => ByteString -> Either String a
  fromHAL = Servant.eitherDecodeLenient


instance Accept HAL where
  contentTypes _ = "application" // "vnd.dwolla.v1.hal+json" :| []

instance FromHAL a => MimeUnrender HAL a where
  mimeUnrender _ b = fromHAL b

instance ToJSON a => MimeRender HAL a where
  mimeRender _ = encode
