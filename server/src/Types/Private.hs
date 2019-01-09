{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
module Types.Private where


import Database.Selda.SqlType (Lit(..), SqlType(..))
import Data.Aeson (ToJSON(..), Value(Null))
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)


newtype Private a = Private
    { private :: a }
    deriving (Show, Eq, Generic)

instance ToJSON (Private a) where
    toJSON _ = Null

instance (Typeable a, SqlType a) => SqlType (Private a) where
    mkLit (Private a) = LCustom $ mkLit a
    sqlType _ = sqlType (Proxy :: Proxy a)
    fromSql v = Private (fromSql v)
    defaultValue = LCustom (defaultValue :: Lit a)


