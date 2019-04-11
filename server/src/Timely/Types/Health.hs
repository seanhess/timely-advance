{-# LANGUAGE DeriveGeneric #-}
module Timely.Types.Health where

import           Data.Aeson                         (ToJSON)
import           Data.Model.Money                   (Money)
import           Data.Number.Abs                    (Abs)
import           Data.Time.Calendar                 (Day)
import           GHC.Generics                       (Generic)
import           Timely.Evaluate.Health.Budget      (Budget)
import           Timely.Evaluate.Health.Transaction (Expense, Income, Transaction)

-- this is the information reported to the user
data AccountHealth = AccountHealth
  { balance   :: Money
  , budgeted  :: Abs Money
  , spending  :: Money
  , income    :: Budget Income
  , bills     :: [Bill]

  , paychecks :: [Transaction Income]
  } deriving (Show, Eq, Generic)

instance ToJSON AccountHealth


data Bill = Bill
  { saved  :: Abs Money
  , next   :: Day
  , budget :: Budget Expense
  } deriving (Show, Eq, Generic)

instance ToJSON Bill
