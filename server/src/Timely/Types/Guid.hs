{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving     #-}
module Timely.Types.Guid
    ( Guid
    , randomId
    , Data.UUID.toText
    , Data.UUID.fromText
    , Data.UUID.toString
    , Data.UUID.fromString
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (fromMaybe)
import           Data.String            as String (IsString (..))
import           Data.UUID              (UUID, fromString, fromText, toString, toText)
import qualified Data.UUID              as UUID
import qualified Data.UUID.V4           as UUID
import           Database.Selda         (SqlType (..))
import           Database.Selda.SqlType (Lit (..), SqlTypeRep (..), SqlValue (..))

type Guid s = UUID
               -- deriving (Generic, Show, Eq, SqlType)

instance SqlType UUID where
    mkLit t = LCustom $ LText $ UUID.toText t
    sqlType _ = TText
    fromSql (SqlString x) =
      fromMaybe (nonUuidError x) (UUID.fromText x)
    fromSql v = nonUuidError v
    defaultValue = LCustom $ LText $ UUID.toText UUID.nil


instance IsString (Guid s) where
  fromString s = fromMaybe (nonUuidError s) $ UUID.fromString s


nonUuidError v = error $ "fromSql: text column with non-text value: " ++ show v


-- instance ToJSON (Id s)
-- instance FromJSON (Id s)


randomId :: MonadIO m => m (Guid s)
randomId = liftIO UUID.nextRandom
