{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
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
    Valid.length 9 t
    Valid.digits t
    pure $ Valid t


data Phone
instance Validate Phone where
  validate t = do
    Valid.length 10 t
    Valid.digits t
    pure $ Valid t





-- Addresses -------------------------------


data State
instance Validate State where
  validate t = do
    Valid.length 2 t
    Valid.alpha t
    pure $ Valid $ Text.toUpper t


data PostalCode
instance Validate PostalCode where
  validate t = do
    Valid.length 5 t
    Valid.digits t
    pure $ Valid t


data Address = Address
  { street1    :: Text
  , street2    :: Maybe Text
  , city       :: Text
  , state      :: Valid State
  , postalCode :: Valid PostalCode
  } deriving (Show, Eq, Generic)

instance ToJSON Address
instance FromJSON Address
