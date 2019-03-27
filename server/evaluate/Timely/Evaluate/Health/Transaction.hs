module Timely.Evaluate.Health.Transaction where

import Data.Model.Money (Money)
import Data.Time.Calendar (Day)
import Timely.Evaluate.Health.Abs (Abs)

data Transaction a = Transaction
  { amount :: Abs Money
  , date :: Day
  } deriving (Show, Eq)

