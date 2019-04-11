module Timely.Evaluate.Offer where


import qualified Data.List              as List
import qualified Data.Maybe             as Maybe
import Data.Number.Abs (Abs, absolute)
import           Data.Model.Money       as Money
import           Data.Time.Clock        (NominalDiffTime, UTCTime (..))
import qualified Data.Time.Clock        as Time
import           Timely.Advances        (Advance (..))

-- If they have any active advances, the trigger amount changes to 250.00
-- If they have any recent advances, don't advance
-- If they have any unclaimed offers at all, don't advance


data Projection = Projection
  { expenses :: Money
  , available :: Money
  }


isNeeded :: Maybe Advance -> [Advance] -> Projection -> UTCTime -> Maybe (Abs Money)
isNeeded offer active health today
  | isAnyRecent today $ advanceTimes offer active = Nothing
  | Maybe.isJust offer = Nothing
  | available health > expenses health = Nothing
  | otherwise = Just $ absolute $ expenses health - available health


-- triggerAmount :: [Advance] -> Money
-- triggerAmount [] = Money.fromFloat 500.00
-- triggerAmount _  = Money.fromFloat 250.00


advanceTimes :: Maybe Advance -> [Advance] -> [UTCTime]
advanceTimes offer active =
  Maybe.catMaybes $ (fmap offered offer : map activated active)


isAnyRecent :: UTCTime -> [UTCTime] -> Bool
isAnyRecent today times =
  List.any (isRecentTime today) times


isRecentTime :: UTCTime -> UTCTime -> Bool
isRecentTime today t = today < Time.addUTCTime intervalRecent t


intervalRecent :: NominalDiffTime
intervalRecent = 1 * Time.nominalDay


