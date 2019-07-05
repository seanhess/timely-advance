{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Timely.Accounts.Types.Subscription where

import Data.Aeson       (FromJSON, ToJSON)
import Data.Model.Money as Money (Money, fromFloat)
import Database.Selda   (SqlType)
import GHC.Generics     (Generic)

data Subscription = Subscription
    { cost  :: Money
    , limit :: Money
    , level :: Level
    } deriving (Show, Eq, Generic)

instance ToJSON Subscription
instance FromJSON Subscription


data Level
    = Basic
    | Premium
    deriving (Show, Eq, Generic, Bounded, Enum, Read)

instance FromJSON Level
instance ToJSON Level
instance SqlType Level

data Request = Request
    { level :: Level
    } deriving (Show, Eq, Generic)

instance FromJSON Request


fromLevel :: Level -> Subscription
fromLevel Basic = Subscription
    { cost = Money.fromFloat 1.00
    , limit = Money.fromFloat 35.00
    , level = Basic
    }
fromLevel Premium = Subscription
    { cost = Money.fromFloat 4.95
    , limit = Money.fromFloat 100
    , level = Premium
    }

levels :: [Level]
levels = [minBound..maxBound]
