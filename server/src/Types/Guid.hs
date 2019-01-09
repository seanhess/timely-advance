-- {-# LANGUAGE DeriveGeneric     #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving     #-}
module Types.Guid where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (replicateM)
import Data.Aeson (ToJSON, FromJSON)
import Data.String.Conversions (cs)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.Selda (SqlType(..))
import Database.Selda.SqlType (Lit(..), SqlValue(..), SqlTypeRep(..))
import GHC.Generics (Generic)
import System.Random (randomIO)

type Guid s = UUID
               -- deriving (Generic, Show, Eq, SqlType)

instance SqlType UUID where
    mkLit t = LCustom $ LText $ UUID.toText t
    sqlType _ = TText
    fromSql (SqlString x) =
      fromMaybe (nonUuidError x) (UUID.fromText x)
    fromSql v = nonUuidError v
    defaultValue = LCustom $ LText $ UUID.toText UUID.nil


nonUuidError v = error $ "fromSql: text column with non-text value: " ++ show v


-- instance ToJSON (Id s)
-- instance FromJSON (Id s)


randomId :: MonadIO m => m (Guid s)
randomId = liftIO UUID.nextRandom

