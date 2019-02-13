{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Model.Digits where


import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Char              as Char
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Typeable          (Typeable)
import           Database.Selda         as Selda
import           Database.Selda.SqlType (Lit (..))
import           GHC.Generics           (Generic)
import           Web.HttpApiData        (FromHttpApiData, ToHttpApiData)


newtype Digits a = Digits Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)


digits :: Text -> Digits a
digits = Digits . Text.filter Char.isDigit


instance Typeable t => SqlType (Digits t) where
    mkLit (Digits t) =  LCustom $ mkLit t
    sqlType _ = sqlType (Proxy :: Proxy Text)
    fromSql v = Digits $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Text)
