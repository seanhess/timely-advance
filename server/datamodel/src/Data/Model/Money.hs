{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Model.Money where

import Data.Aeson             (FromJSON, ToJSON)
import Numeric           (showFFloat)
import Data.Proxy             (Proxy (..))
import Data.Text              (Text, pack)
import Data.Typeable          (Typeable)
import Database.Selda.SqlType (Lit (..), SqlType (..))
import GHC.Generics           (Generic)

newtype Money = Money Int
    deriving (Generic, Eq, Show, Typeable, Num, Ord, ToJSON, FromJSON)

-- This does NOT work. Calculating percentages is all wonkey. It comes in as 50 cents, which doesn't make any sense.
-- instance Fractional Money where
--   fromRational = fromFloat . fromRational
--   a / b = fromFloat (toFloat a / toFloat b)


money :: Float -> Money
money = fromFloat


fromFloat :: Float -> Money
fromFloat f = Money $ round (f * 100)

toFloat :: Money -> Float
toFloat (Money i) = fromIntegral i / 100

toCents :: Money -> Int
toCents (Money i) = i

fromCents :: Int -> Money
fromCents i = Money i

instance SqlType Money where
    mkLit (Money b) =  LCustom $ mkLit b
    sqlType _ = sqlType (Proxy :: Proxy Int)
    fromSql v = Money $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Int)


formatFloat :: Money -> Text
formatFloat m = pack $ showFFloat (Just 2) (toFloat m) ""

formatCents :: Money -> Text
formatCents m = pack $ show (toCents m)
