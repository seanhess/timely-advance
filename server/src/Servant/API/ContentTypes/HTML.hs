{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.ContentTypes.HTML where

import Data.String.Conversions (cs)
import Data.Aeson (encode, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import GHC.TypeLits (KnownSymbol, symbolVal, Symbol)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Servant.API.ContentTypes


data HTML


data Link = Link
    { _self :: Text
    , _relations :: [Text]
    } deriving (Generic)

instance ToJSON Link


-- it's more "resources"
instance Linkable Link where
    self (Link a _) = a
    relations (Link _ rs) = rs


class Linkable a where
    self      :: a -> Text
    relations :: a -> [Text]
    relations _ = []


instance Accept HTML where
   contentType _ = "text" // "html" /: ("charset", "utf-8")


-- If it's a list I want it to use this
instance (ToJSON a, Linkable a) => MimeRender HTML a where
    mimeRender _ val = cs $ row $ cols val


instance {-# OVERLAPS #-} (MimeRender HTML a) => MimeRender HTML [a] where
    mimeRender p val = mconcat $ map (mimeRender p) val



-- table :: [Text] -> Text
-- table rows = "<table>" <> mconcat rows <> "</table>"

row :: [Text] -> Text
row cols = "<p>" <> mconcat cols <> "</p>"

cols :: (ToJSON a, Linkable a) => a -> [Text]
cols a = [idCol a, jsonCol a]

idCol :: Linkable a => a -> Text
idCol a = "<div width='100%'>" <> mconcat (map link links) <> "</div>"
  where
    links = selfLink : map relLink (relations a)
    selfLink = [self a]
    relLink rel = [self a, rel]

link :: [Text] -> Text
link segments =
    "<span style='padding-right: 5px'><a href='"<> url <> "'>" <> url <> "</a></span>"
  where
    url :: Text
    url = mconcat (List.intersperse "/" segments)

jsonCol :: ToJSON a => a -> Text
jsonCol a = "<div><pre>" <> cs (encodePretty a) <> "</pre></div>"




