{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Model.Guid
    ( Guid
    , randomId
    , toText
    , fromText
    , toString
    , Data.Model.Guid.fromString
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Maybe             (fromMaybe)
import           Data.String            as String (IsString (..))
import qualified Data.String            as String
import           Data.Text              (Text)
import           Data.Typeable          (Typeable)
import           Data.UUID              (UUID)
import qualified Data.UUID              as UUID
import qualified Data.UUID.V4           as UUID
import           Database.Selda         (SqlType (..))
import           Database.Selda.SqlType (Lit (..), SqlTypeRep (..), SqlValue (..))
import           GHC.Generics           (Generic)
import           Web.HttpApiData        (FromHttpApiData, ToHttpApiData)

newtype Guid s = Guid UUID
    deriving (Generic, Show, Eq, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)

instance Typeable s => SqlType (Guid s) where
    mkLit (Guid t) = LCustom $ LText $ UUID.toText t
    sqlType _ = TText
    fromSql (SqlString x) =
      fromMaybe (nonUuidError x) (Guid <$> UUID.fromText x)
    fromSql v = nonUuidError v
    defaultValue = LCustom $ LText $ UUID.toText UUID.nil


instance IsString (Guid s) where
  fromString s = Guid <$> fromMaybe (nonUuidError s) $ UUID.fromString s


toUUID :: Guid s -> UUID
toUUID (Guid u) = u

toText :: Guid s -> Text
toText = UUID.toText . toUUID

fromText :: Text -> Maybe (Guid s)
fromText = fmap Guid . UUID.fromText

toString :: Guid s -> String
toString = UUID.toString . toUUID

fromString :: String -> Maybe (Guid s)
fromString = fmap Guid . UUID.fromString


nonUuidError v = error $ "fromSql: text column with non-text value: " ++ show v


randomId :: MonadIO m => m (Guid s)
randomId = Guid <$> liftIO UUID.nextRandom
