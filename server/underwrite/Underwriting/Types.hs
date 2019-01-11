module Underwriting.Types where

import GHC.Generics (Generic)

import Types.Money


data Result
   = Approved Approval
   | Denied Denial
   deriving (Generic, Show, Eq)

data Denial = Denial
    deriving (Generic, Show, Eq, Bounded, Enum, Read)

data Approval = Approval
    { approvalAmount :: Money
    } deriving (Generic, Show, Eq)


