{-# LANGUAGE DeriveGeneric #-}
module Timely.Evaluate.Health.Event where

import Data.Aeson                         (ToJSON)
import Data.Model.Money                   (Money)
import GHC.Generics                       (Generic)
import Timely.Evaluate.Health.Budget      (Budget)
import Timely.Evaluate.Health.Transaction (Any, Transaction)

data Event = Event
  { transaction :: Transaction Any
  , balance     :: Money
  , budget      :: Budget Any
  } deriving (Show, Eq, Generic)

instance ToJSON Event
