module Timely.Accounts.Types.Api where

import Data.Aeson (ToJSON(..))
import Database.Selda (ID, fromId)

instance ToJSON (ID a) where
    toJSON i = toJSON $ fromId i
