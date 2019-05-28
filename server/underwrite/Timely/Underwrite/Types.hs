{-# LANGUAGE DeriveGeneric #-}
module Timely.Underwrite.Types where

import Data.Model.Money
import GHC.Generics     (Generic)


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

