{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Timely.Accounts.Types.Application where


import Data.Aeson                    (FromJSON (..), ToJSON (..))
import Data.Model.Guid               (Guid)
import Data.Model.Id                 (Id (..), Token)
import Data.Model.Types              (Phone, SSN)
import Data.Model.Valid              (Valid (..))
import Data.Text                     as Text
import Data.Time.Calendar            (Day)
import Data.Time.Clock               (UTCTime)
import Database.Selda                (SqlRow (..))
import Database.Selda.SqlType        (SqlType (..))
import GHC.Generics                  (Generic)
import Timely.Accounts.Types.Account (Account)
import Timely.Accounts.Types.Api     ()
import Timely.Bank                   (Item, Public)


-- Can we have more than one application per phone number? NO
data Application = Application
    { accountId       :: Guid Account
    , phone           :: Valid Phone
    , email           :: Text
    , ssn             :: Valid SSN
    , dateOfBirth     :: Day
    , publicBankToken :: Token Public
    , created         :: UTCTime
    , onboarding      :: Onboarding
    } deriving (Generic, Show)

instance SqlRow Application
instance ToJSON Application
instance FromJSON Application


data Onboarding
    = Pending
    | Complete
    | Error
    | RejectedIncomeLow
    | RejectedIncomeNotRegular
    deriving (Show, Eq, Generic, Read, Bounded, Enum)
instance SqlType Onboarding
instance ToJSON Onboarding
instance FromJSON Onboarding


-- save this as soon as we have the bank id
data AppBank = AppBank
    { accountId    :: Guid Account
    , bankItemId   :: Id Item
    , transactions :: Maybe Int
    } deriving (Generic, Show)

instance SqlRow AppBank
