{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.ContentTypes.HTML where

import Data.String.Conversions (cs)
import Data.Aeson (encode, ToJSON)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, symbolVal, Symbol)
import Network.HTTP.Media ((//), (/:))
import Servant.API.ContentTypes


data HTML


class Linkable a where
    linkId :: a -> Text


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
idCol a = "<div width='100%'><a href='"<> linkId a <> "'>" <> linkId a <> "</a></div>"

jsonCol :: ToJSON a => a -> Text
jsonCol a = "<div><pre>" <> cs (encode a) <> "</pre></div>"




