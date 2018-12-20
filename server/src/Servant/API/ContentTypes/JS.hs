{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.ContentTypes.JS where

import Data.String.Conversions (cs)
import Data.Aeson (encode, ToJSON)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal, Symbol)
import Network.HTTP.Media ((//), (/:))
import Servant.API.ContentTypes

data JS (s :: Symbol)

instance Accept (JS s) where
   contentType _ = "text" // "javascript" /: ("charset", "utf-8")

instance (ToJSON a, KnownSymbol s) => MimeRender (JS s) a where
   mimeRender _ val = "var " <> cs var <> " = " <> encode val <> ";"
    where
      var :: String
      var = symbolVal (Proxy :: Proxy s)

