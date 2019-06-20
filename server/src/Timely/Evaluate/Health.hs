{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Timely.Evaluate.Health
  ( timeline
  , dailyBalances
  , minimumBalance
  , isBudget
  , Daily
  , DailyBalance
  , Budget
  , Scheduled
  , Transaction
  , Income, Expense
  ) where

import Timely.Evaluate.Health.Budget
import Timely.Evaluate.Health.Scheduled
import Timely.Evaluate.Health.Daily
import Timely.Evaluate.Health.Timeline
import Timely.Evaluate.Health.Transaction



isBudget :: forall a. Budget a -> Transaction a -> Bool
isBudget b t =
  name (b :: Budget a) == name (t :: Transaction a)
