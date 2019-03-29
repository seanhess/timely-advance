{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Timely.Accounts.Types.Health where


import           Data.Aeson                    (ToJSON)
import           Data.Model.Guid               (Guid)
import           Data.Model.Money              as Money
import           Data.Time.Clock               (UTCTime)
import           Database.Selda                (SqlRow (..))
import           GHC.Generics                  (Generic)
import           Timely.Accounts.Types.Account (Account)

-- Account Projection -------------------


data Health = Health
    { accountId :: Guid Account
    , expenses  :: Money
    , available :: Money
    , created   :: UTCTime
    } deriving (Show, Eq, Generic)

instance SqlRow Health
instance ToJSON Health


