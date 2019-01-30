{-# LANGUAGE RecordWildCards #-}
module Timely.Evaluate.AccountHealth where

import qualified Data.List             as List

import           Timely.Advances       (Advance (..))
import           Timely.Evaluate.Types (Projection (..))
import           Timely.Types.Money    (Money)
import qualified Timely.Types.Money    as Money


-- active advances?
-- TODO we need to know / calculate their projected expenses.
data Info = Info
    { checkingBalance :: Money
    , activeAdvances  :: [Advance]
    }




-- TODO more information on "expenses"? Like which expenses do we predict will happen again, etc

-- | Decide what to do given the advance needed and their account state
analyze :: Money -> [Advance] -> Projection
analyze checking advances =
    Projection
      { expenses  = projectedSpending
      , available = checking
      , advances  = List.sum $ List.map amount advances
      }


-- checkCredit :: Money -> Money -> Health
-- checkCredit credit amt
--   | amt <= credit = Needs amt
--   | otherwise     = Maxed amt credit


-- -- plus saftey margin?
-- advanceNeeded :: Money -> Money -> Maybe Money
-- advanceNeeded balance projected = do
--     short <- shortfall balance projected
--     pure $ short + safetyMargin






-- TODO inputs
projectedSpending :: Money
projectedSpending = Money.fromFloat 200.00


-- in case we mis-predict
-- TODO base this on possible unknown transaction size? By analyzing their account?
-- safetyMargin :: Money
-- safetyMargin = Money.fromFloat 100.00



-- shortfall :: Money -> Money -> Maybe Money
-- shortfall balance projected = do
--     guard (projected > balance)
--     pure $ projected - balance



