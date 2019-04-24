{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.Model.Guid
    ( Guid
    , randomId
    , toText
    , fromText
    , toString
    , Data.Model.Guid.fromString
    , GuidPrefix(..)
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import qualified Data.Aeson             as Aeson
import           Data.Maybe             (fromMaybe, listToMaybe)
import           Data.Model.Random      (randomAZ)
import           Data.Proxy             (Proxy (..))
import           Data.String            as String (IsString (..))
import qualified Data.String            as String
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Typeable          (Typeable, typeOf)
import           Data.UUID              (UUID)
import qualified Data.UUID              as UUID
import qualified Data.UUID.V4           as UUID
import           Database.Selda         (SqlType (..))
import           Database.Selda.SqlType (Lit (..), SqlTypeRep (..), SqlValue (..))
import           GHC.Generics           (Generic)
import           Web.HttpApiData        (FromHttpApiData (..), ToHttpApiData (..))

newtype Guid s = Guid Text
  deriving (Generic, Eq)

class GuidPrefix s where
  guidPrefix :: Proxy s -> Text

  default guidPrefix :: Typeable s => Proxy s -> Text
  guidPrefix _ = Text.toLower $ Text.pack $ show $ typeOf (undefined :: s)

instance GuidPrefix s => Show (Guid s) where
  show = Text.unpack . toText

instance GuidPrefix s => ToJSON (Guid s) where
  toJSON = Aeson.String . toText

instance FromJSON (Guid s) where
  parseJSON v = fromText <$> parseJSON v

instance GuidPrefix s => ToHttpApiData (Guid s) where
  toUrlPiece = toText

instance FromHttpApiData (Guid s) where
  parseUrlPiece = Right . fromText

instance (GuidPrefix s, Typeable s) => SqlType (Guid s) where
    mkLit g = LCustom $ LText $ toText g
    sqlType _ = TText
    fromSql (SqlString x) = fromText x
    fromSql v             = nonUuidError v
    defaultValue = LCustom $ LText $ UUID.toText UUID.nil


instance IsString (Guid s) where
  fromString s = fromText $ Text.pack s


toText :: forall s. GuidPrefix s => Guid s -> Text
toText (Guid s) = Text.intercalate "-"
   [ guidPrefix (Proxy :: Proxy s)
   , s
   ]

fromText :: Text -> Guid s
fromText = Guid . parseText

toString :: GuidPrefix s => Guid s -> String
toString = Text.unpack . toText

fromString :: String -> Guid s
fromString = Guid . parseText . Text.pack


parseText :: Text -> Text
parseText = fromMaybe "" . listToMaybe . reverse . Text.splitOn "-"




nonUuidError v = error $ "fromSql: text column with non-text value: " ++ show v


randomId :: MonadIO m => m (Guid s)
randomId = Guid <$> randomAZ 20
