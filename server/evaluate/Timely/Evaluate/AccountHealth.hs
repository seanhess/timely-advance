{-# LANGUAGE RecordWildCards #-}
module Timely.Evaluate.AccountHealth where

import           Control.Monad (guard)
import           Data.Time.Calendar (Day)

import           Timely.Types.Money (Money)
import qualified Timely.Types.Money as Money


-- active advances?
data Info = Info
    { approval :: Money
    , checkingBalance :: Money
    , activeAdvances  :: [Advance]
    }

-- TODO other fields?
data Advance = Advance
    { amount :: Money
    , due :: Day
    }


type Credit = Money


data Health
    = Ok
    | Needs Money
    | Maxed Money Credit



-- | Decide what to do given the advance needed and their account state
analyze :: Info -> Health
analyze Info {..} =
    let projected = projectedSpending
        advance   = advanceNeeded checkingBalance projected
        credit    = creditRemaining approval activeAdvances
    in maybe Ok (checkCredit credit) advance


checkCredit :: Money -> Money -> Health
checkCredit credit amt
  | amt <= credit = Needs amt
  | otherwise     = Maxed amt credit


advanceNeeded :: Money -> Money -> Maybe Money
advanceNeeded balance projected = do
    short <- shortfall balance projected
    pure $ short + safetyMargin


creditRemaining :: Money -> [Advance] -> Money
creditRemaining approval active =
    approval - (sum $ map amount active)





-- TODO inputs
projectedSpending :: Money
projectedSpending = Money.fromFloat 200.00


-- in case we mis-predict
-- TODO base this on possible unknown transaction size? By analyzing their account?
safetyMargin :: Money
safetyMargin = Money.fromFloat 100.00



shortfall :: Money -> Money -> Maybe Money
shortfall balance projected = do
    guard (projected > balance)
    pure $ projected - balance






