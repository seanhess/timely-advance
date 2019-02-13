{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module Data.Model.Valid where


import           Data.Aeson             (FromJSON (..), ToJSON)
import qualified Data.Aeson             as Aeson
import qualified Data.Char              as Char
import Control.Monad.Fail (MonadFail)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Typeable          (Typeable, typeRep)
import           Database.Selda         as Selda
import           Database.Selda.SqlType (Lit (..))
import           GHC.Generics           (Generic)
import           Web.HttpApiData        (FromHttpApiData(..), ToHttpApiData)



newtype Valid a = Valid { valid :: Text }
  deriving (Show, Eq, Generic, ToJSON, ToHttpApiData)


instance Validate a => FromJSON (Valid a) where
  parseJSON = Aeson.withText msg $ \t ->
      case validate t of
        Nothing -> fail $ msg ++ ": " ++ Text.unpack t
        Just v  -> pure v
    where msg = expects (Proxy :: Proxy a)


instance Validate a => FromHttpApiData (Valid a) where
  parseUrlPiece t =
    case validate t of
      Nothing -> fail $ msg ++ ": " ++ Text.unpack t
      Just v  -> pure v
    where msg = expects (Proxy :: Proxy a)




-- so you provide the error message here
class Validate a where
  validate :: Text -> Maybe (Valid a)
  expects  :: Proxy a -> String
  default expects :: Typeable a => Proxy a -> String
  expects p = show $ typeRep p



instance Typeable t => SqlType (Valid t) where
    mkLit (Valid t) =  LCustom $ mkLit t
    sqlType _ = sqlType (Proxy :: Proxy Text)
    fromSql v = Valid $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Text)


isDigits :: Text -> Bool
isDigits = Text.all Char.isDigit

isLength :: Int -> Text -> Bool
isLength n t = Text.length t == n
