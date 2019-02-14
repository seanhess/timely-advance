{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Model.Money where

import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Proxy             (Proxy (..))
import           Data.Typeable          (Typeable)
import           Database.Selda.SqlType (Lit (..), SqlType (..))
import           GHC.Generics           (Generic)

newtype Money = Money Int
    deriving (Generic, Eq, Show, Typeable, Num, Ord, ToJSON, FromJSON)

fromFloat :: Float -> Money
fromFloat f = Money $ round (f * 100)

toFloat :: Money -> Float
toFloat (Money i) = fromIntegral i / 100

instance SqlType Money where
    mkLit (Money b) =  LCustom $ mkLit b
    sqlType _ = sqlType (Proxy :: Proxy Int)
    fromSql v = Money $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Int)