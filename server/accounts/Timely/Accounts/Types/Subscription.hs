{-# LANGUAGE DeriveGeneric #-}
module Timely.Accounts.Types.Subscription where

import Data.Aeson       (FromJSON, ToJSON)
import Data.Model.Money as Money (Money, fromFloat)
import GHC.Generics     (Generic)

data Subscription = Subscription
    { cost  :: Money
    , limit :: Money
    } deriving (Show, Eq, Generic)

instance ToJSON Subscription
instance FromJSON Subscription


data SubscriptionLevel
    = Basic
    | Approved
    deriving (Show, Eq, Generic)

instance FromJSON SubscriptionLevel

data SubscriptionLevelRequest = SubscriptionLevelRequest
    { level :: SubscriptionLevel
    } deriving (Show, Eq, Generic)

instance FromJSON SubscriptionLevelRequest


fromLevel :: SubscriptionLevel -> Subscription
fromLevel Basic = Subscription
    { cost = Money.fromFloat 1.00
    , limit = Money.fromFloat 35.00
    }
fromLevel Approved = Subscription
    { cost = Money.fromFloat 4.95
    , limit = Money.fromFloat 100
    }
