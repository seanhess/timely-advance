module Network.Experian.Address where

import Data.Text (Text)

data Address = Address
  { line1   :: Text
  , line2   :: Text
  , city    :: Text
  , state   :: State
  , zipCode :: ZipCode
  }

newtype State = State Text
  -- "CA"

newtype ZipCode = ZipCode Text
  --  "915021234"
