{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Timely.Evaluate.Health.Budget where


import Data.Aeson               (FromJSON, ToJSON)
import Data.Model.Guid          as Guid (Guid, GuidPrefix (..), fromText, toText)
import Data.Model.Meta          as Meta (Meta (..), meta, value)
import Data.Model.Money         (Money)
import Data.Number.Abs          (Abs)
import Data.Text                (Text)
import GHC.Generics             (Generic)
import Timely.Evaluate.Schedule (Schedule)


newtype Budget a = Budget (Meta "budgetId" (Guid (Budget a)) (BudgetInfo a))
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


budgetId :: Budget a -> Guid (Budget a)
budgetId (Budget m) = Meta.meta m


budget :: Budget a -> BudgetInfo a
budget (Budget m) = Meta.value m


instance GuidPrefix (Budget a) where
  guidPrefix _ = "bgt"


data BudgetInfo a = BudgetInfo
  { name     :: Text
  , schedule :: Schedule
  , amount   :: Abs Money
  } deriving (Show, Eq, Generic)

instance FromJSON (BudgetInfo a)
instance ToJSON (BudgetInfo a)


convert :: BudgetInfo a -> BudgetInfo b
convert (BudgetInfo a b c) = BudgetInfo a b c


convertBoth :: Budget a -> Budget b
convertBoth (Budget (Meta m v) ) = Budget $ Meta (convertId m) (convert v)


convertId :: Guid (Budget a) -> Guid (Budget b)
convertId = Guid.fromText . Guid.toText
