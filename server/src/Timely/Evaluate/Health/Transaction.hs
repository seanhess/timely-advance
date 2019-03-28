module Timely.Evaluate.Health.Transaction where

import           Data.Model.Money   (Money)
import           Data.Number.Abs    (Abs)
import           Data.Time.Calendar (Day)

data Transaction a = Transaction
  { amount :: Abs Money
  , date   :: Day
  } deriving (Show, Eq)
