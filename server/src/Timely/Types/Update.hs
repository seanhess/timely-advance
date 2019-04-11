{-# LANGUAGE DeriveGeneric #-}
module Timely.Types.Update where


import           Control.Exception     (Exception)
import           Data.Aeson            (Options (..), SumEncoding (..), ToJSON (..), defaultOptions, genericToJSON)
import           Data.Model.Guid       (Guid)
import           GHC.Generics          (Generic)
import           Timely.Accounts.Types (Account)

data Error
  = NoIncome   (Guid Account)
  | NoChecking (Guid Account)
  deriving (Show, Eq, Generic)

instance ToJSON Error where
  toJSON = genericToJSON defaultOptions { sumEncoding = TaggedObject "error" "info" }

instance Exception Error
