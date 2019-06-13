{-# LANGUAGE DeriveGeneric #-}
module Timely.Evaluate.Health.Budget where


import Data.Aeson               (FromJSON, ToJSON)
import Data.Model.Money         (Money)
import Data.Number.Abs          (Abs (..))
import Data.Text                (Text)
import Data.Time.Calendar       (Day)
import GHC.Generics             (Generic)
import Timely.Evaluate.Schedule (Schedule)



data Budget a = Budget
  { name     :: Text
  , schedule :: Schedule
  , amount   :: Abs Money
  } deriving (Show, Eq, Generic)

instance ToJSON (Budget a)
instance FromJSON (Budget a)


data Scheduled a = Scheduled
  { budget :: Budget a
  , date   :: Day
  } deriving (Show, Eq, Generic)

instance ToJSON (Scheduled a)
instance FromJSON (Scheduled a)

