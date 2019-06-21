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
  , BudgetInfo
  , Scheduled
  , Transaction
  , Income, Expense
  ) where

import Timely.Evaluate.Health.Budget
import Timely.Evaluate.Health.Scheduled
import Timely.Evaluate.Health.Daily
import Timely.Evaluate.Health.Timeline
import Timely.Evaluate.Health.Transaction



isBudget :: forall a. BudgetInfo a -> Transaction a -> Bool
isBudget b t =
  name (b :: BudgetInfo a) == name (t :: Transaction a)
