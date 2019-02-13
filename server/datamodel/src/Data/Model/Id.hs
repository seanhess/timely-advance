{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Model.Id where


import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Data.Typeable          (Typeable)
import           Database.Selda         as Selda
import           Database.Selda.SqlType (Lit (..))
import           GHC.Generics           (Generic)
import           Web.HttpApiData        (FromHttpApiData, ToHttpApiData)


newtype Token t = Token Text
    deriving (Generic, FromJSON, ToJSON, Show, Eq, Typeable, ToHttpApiData, FromHttpApiData)

newtype Id t = Id Text
    deriving (Generic, FromJSON, ToJSON, Show, Eq, Typeable, ToHttpApiData, FromHttpApiData)


instance Typeable t => SqlType (Token t) where
    mkLit (Token t) =  LCustom $ mkLit t
    sqlType _ = sqlType (Proxy :: Proxy Text)
    fromSql v = Token $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Text)

instance Typeable t => SqlType (Id t) where
    mkLit (Id t) =  LCustom $ mkLit t
    sqlType _ = sqlType (Proxy :: Proxy Text)
    fromSql v = Id $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Text)
