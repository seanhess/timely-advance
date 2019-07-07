{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Timely.Accounts.Types.Account where


import Data.Aeson               (FromJSON (..), ToJSON (..))
import Data.Model.Guid          (Guid, GuidPrefix (..))
import Data.Model.Id            (Id (..), Token (..))
import Data.Model.Types         (Phone)
import Data.Model.Valid         as Valid
import Database.Selda           as Selda
import GHC.Generics             (Generic)
import Timely.Bank              (Access, Item)
import Timely.Transfers.Account (TransferAccount)




data Account = Account
    { accountId  :: Guid Account
    , phone      :: Valid Phone
    , transferId :: Id TransferAccount
    , bankToken  :: Token Access
    , bankItemId :: Id Item
    , created    :: UTCTime
    } deriving (Generic, Eq, Show)

instance SqlRow Account
instance ToJSON Account
instance FromJSON Account
instance GuidPrefix Account where
  guidPrefix _ = "acc"














