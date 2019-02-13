{-# LANGUAGE DeriveGeneric #-}
module Data.Model.Types
  ( SSN
  , Phone
  , State
  , PostalCode
  , Address(..)
  , Valid(..)
  , Validate(..)
  ) where

import           Control.Monad    (guard)
import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Char        as Char
import           Data.Model.Valid as Valid
import           Data.Text        as Text
import           GHC.Generics     (Generic)



-- Personal Information -------------------

data SSN
instance Validate SSN where
  validate t = do
    guard (Text.length t == 9)
    guard (Text.all Char.isDigit t)
    pure $ Valid t


data Phone
instance Validate Phone where
  validate t = do
    guard (Text.length t == 10)
    guard (Text.all Char.isDigit t)
    pure $ Valid t





-- Addresses -------------------------------


data State
instance Validate State where
  validate t = do
    guard (Text.length t == 2)
    guard (Text.all Char.isAlpha t)
    pure $ Valid $ Text.toUpper t


data PostalCode
instance Validate PostalCode where
  validate t = do
    guard (Text.length t == 5)
    guard (Text.all Char.isDigit t)
    pure $ Valid t


data Address = Address
  { address1   :: Text
  , address2   :: Maybe Text
  , city       :: Text
  , state      :: Valid State
  , postalCode :: Valid PostalCode
  } deriving (Show, Eq, Generic)

instance ToJSON Address
instance FromJSON Address
