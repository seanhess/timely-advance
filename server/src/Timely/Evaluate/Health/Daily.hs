{-# LANGUAGE DeriveGeneric #-}
module Timely.Evaluate.Health.Daily where

import Data.Aeson                         (ToJSON)
import Data.Model.Money                   (Money)
import Data.Number.Abs                    (Abs (..))
import Data.Time.Calendar                 (Day)
import GHC.Generics                       (Generic)
import Timely.Evaluate.Health.Budget      (Budget)
import Timely.Evaluate.Health.Transaction (Expense)


data Daily = Daily
  { date     :: Day
  , spending :: Abs Money
  , bills    :: [Budget Expense]
  } deriving (Show, Eq, Generic)

instance ToJSON Daily

-- | The Daily spending applied to a balance
data DailyBalance = DailyBalance
  { daily   :: Daily
  , balance :: Money
  } deriving (Show, Eq, Generic)

instance ToJSON DailyBalance
