{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Timely.Evaluate.Health
  ( Timeline(..)
  , Event(..)
  , timeline
  , isBudget
  , Budget
  , Transaction
  , Income, Expense
  ) where

import Timely.Evaluate.Health.Budget
import Timely.Evaluate.Health.Event
import Timely.Evaluate.Health.Timeline
import Timely.Evaluate.Health.Transaction



isBudget :: forall a. Budget a -> Transaction a -> Bool
isBudget b t =
  name (b :: Budget a) == name (t :: Transaction a)
