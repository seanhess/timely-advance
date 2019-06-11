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



expense :: Text -> Money -> Day -> Transaction Expense
expense n a d = Transaction n d (abs a)

income :: Text -> Money -> Day -> Transaction Income
income n a d = Transaction n d (abs a)

any :: Text -> Money -> Day -> Transaction a
any n a d = Transaction n d a

