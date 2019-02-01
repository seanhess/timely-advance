module Timely.Evaluate.Offer where


import qualified Data.List             as List
import           Data.Time.Clock       (UTCTime (..))
import qualified Data.Time.Clock       as Time
import           Timely.Advances       (Advance (..))
import           Timely.Evaluate.Types (Projection (..))
import           Timely.Types.Money    as Money


isNeeded :: Maybe Advance -> [Advance] -> Projection -> UTCTime -> Bool
isNeeded offer active health today =
  if isRecent
     then available health < Money.fromFloat 250.00
     else available health < Money.fromFloat 500.00

  where
    isRecent = List.all isRecentTime (fmap offered offer : map activated active)
    isRecentTime (Just t) = today < Time.addUTCTime interval24h t
    isRecentTime Nothing  = False
    interval24h = 1 * Time.nominalDay


amount :: Money
amount = Money.fromFloat 200.00
