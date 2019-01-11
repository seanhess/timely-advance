module Underwriting.Types where

import GHC.Generics (Generic)

import Types.Money


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



