{-# LANGUAGE RecordWildCards #-}
module Timely.Evaluate.AccountHealth where

import           Data.Model.Money      (Money)
import qualified Data.Model.Money      as Money
import           Timely.Evaluate.Types (Projection (..))



-- TODO more information on "expenses"? Like which expenses do we predict will happen again, etc
-- TODO accurate projected spending

analyze :: Money -> Projection
analyze checking =
    Projection
      { expenses  = projectedSpending
      , available = checking
      }

-- TODO inputs
projectedSpending :: Money
projectedSpending = Money.fromFloat 200.00
