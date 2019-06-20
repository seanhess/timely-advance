{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Evaluate.Health.Scheduled where


import Data.Aeson               (FromJSON(..), ToJSON(..), Value(..), object, (.=), withObject, (.:))
import Data.Time.Calendar       (Day)
import GHC.Generics             (Generic)
import Data.HashMap.Strict as HM (union)



data Scheduled a = Scheduled
  { date   :: Day
  , item   :: a
  } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (Scheduled a) where
  toJSON (Scheduled d a) =
    object ["date" .= toJSON d] `merge` toJSON a

instance FromJSON a => FromJSON (Scheduled a) where
  parseJSON = withObject "Scheduled" $ \v -> do
    i <- parseJSON (Object v)
    d <- v .: "date"
    pure $ Scheduled d i


merge :: Value -> Value -> Value
merge (Object o1) (Object o2) = Object $ HM.union o1 o2
merge (Object o1) _ = Object o1
merge _ (Object o2) = Object o2
merge a _ = a




