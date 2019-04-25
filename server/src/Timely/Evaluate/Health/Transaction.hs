{-# LANGUAGE DeriveGeneric #-}
module Timely.Evaluate.Health.Transaction
  ( Transaction(name, date, amount)
  , Any, Expense, Income
  , any, expense, income
  ) where

import Data.Aeson         (ToJSON)
import Data.Model.Money   (Money)
import Data.Text          (Text)
import Data.Time.Calendar (Day)
import GHC.Generics       (Generic)
import Prelude            hiding (any)

data Expense
data Income
data Any

data Transaction a = Transaction
  { name   :: Text
  , date   :: Day
  , amount :: Money
  } deriving (Show, Eq, Generic)

instance ToJSON (Transaction a)



expense :: Text -> Day -> Money -> Transaction Expense
expense n d a = Transaction n d (abs a)

income :: Text -> Day -> Money -> Transaction Income
income n d a = Transaction n d (abs a)

any :: Text -> Day -> Money -> Transaction Any
any n d a = Transaction n d a
