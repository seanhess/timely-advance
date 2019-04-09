{-# LANGUAGE DeriveGeneric #-}
module Timely.Evaluate.Health.Transaction where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import           Data.Model.Money   (Money)
import           Data.Number.Abs    (Abs)
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)


data Expense
data Income

data Transaction a = Transaction
  { name   :: Text
  , amount :: Abs Money
  , date   :: Day
  } deriving (Show, Eq, Generic)

instance ToJSON (Transaction a)
