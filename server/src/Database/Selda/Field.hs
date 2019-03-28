module Database.Selda.Field where

import           Data.Aeson              (FromJSON, ToJSON)
import qualified Data.Aeson              as Aeson
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Typeable           (Typeable)
import           Database.Selda.SqlType  (Lit (..), SqlType (..), SqlTypeRep (..), SqlValue (..))

-- stores are parses things as JSON
newtype Field a = Field { field :: a }
  deriving (Show, Eq)

instance (ToJSON a, FromJSON a, Typeable a) => SqlType (Field a) where
    mkLit (Field a) =  LCustom $ mkLit $ Aeson.encode a
    sqlType _ = TText
    fromSql (SqlString x) =
      case Aeson.eitherDecode (cs x) of
        Left err -> error $ "fromSql: field column with invalid JSON: " ++ err
        Right a  -> Field a
    fromSql v             = error $ "fromSql: field column with non-text value: " ++ show v
    defaultValue = LCustom (defaultValue :: Lit Text)
