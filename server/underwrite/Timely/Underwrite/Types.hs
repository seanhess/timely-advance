{-# LANGUAGE DeriveGeneric #-}
module Timely.Underwrite.Types where

import Data.Model.Money
import Data.Model.Types   (Address (..), Phone, SSN, Valid)
import Data.Text          (Text)
import Data.Time.Calendar (Day)
import GHC.Generics       (Generic)


data Result
   = Approved Approval
   | Denied Denial
   deriving (Generic, Show, Eq)

data Approval = Approval
    { approvalAmount :: Money
    } deriving (Generic, Show, Eq)


data DenialReason = NoReason | SomeReason
    deriving (Generic, Show, Eq, Bounded, Enum, Read)

data Denial = Denial
    { denial :: DenialReason
    } deriving (Generic, Show, Eq)



data Application = Application
    { phone       :: Valid Phone
    , firstName   :: Text
    , middleName  :: Maybe Text
    , lastName    :: Text
    , email       :: Text
    , ssn         :: Valid SSN
    , dateOfBirth :: Day
    , address     :: Address
    }
