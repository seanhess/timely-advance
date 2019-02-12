{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Network.Dwolla.HAL where


import           Servant
import           Data.Aeson               as Aeson
import qualified Servant.API.ContentTypes as Servant
import           Network.HTTP.Media       ((//))
import           Data.List.NonEmpty       (NonEmpty ((:|)))


data HAL


instance Accept HAL where
  contentTypes _ = "application" // "vnd.dwolla.v1.hal+json" :| []

instance FromJSON a => MimeUnrender HAL a where
  mimeUnrender _ b = Servant.eitherDecodeLenient b

instance ToJSON a => MimeRender HAL a where
  mimeRender _ = encode
