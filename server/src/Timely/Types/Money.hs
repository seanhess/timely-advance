{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Timely.Types.Money where

import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Database.Selda.SqlType (SqlType(..), Lit(..))
import GHC.Generics (Generic)

newtype Money = Money Int
    deriving (Generic, Eq, Show, Typeable, Num, Ord)

fromFloat :: Float -> Money
fromFloat f = Money $ round (f * 100)

instance SqlType Money where
    mkLit (Money b) =  LCustom $ mkLit b
    sqlType _ = sqlType (Proxy :: Proxy Int)
    fromSql v = Money $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Int)

