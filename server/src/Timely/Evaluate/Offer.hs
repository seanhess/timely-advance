module Timely.Evaluate.Offer where


import           Data.Function    ((&))
import qualified Data.List        as List
import qualified Data.Maybe       as Maybe
import           Data.Model.Money as Money
import           Data.Number.Abs  (Abs, absolute)
import           Data.Time.Clock  (NominalDiffTime, UTCTime (..))
import qualified Data.Time.Clock  as Time
import           Timely.Advances  (Advance (..))
import qualified Timely.Advances  as Advances

-- If they have any active advances, the trigger amount changes to 250.00
-- If they have any recent advances, don't advance
-- If they have any unclaimed offers at all, don't advance


data Projection = Projection
  { expenses  :: Money
  , available :: Money
  }


isNeeded :: Maybe Advance -> [Advance] -> Projection -> UTCTime -> Bool
isNeeded offer active health today
  | isAnyRecent today $ advanceTimes offer active = False
  | Maybe.isJust offer = False
  | available health > expenses health = False
  | otherwise = True



amount :: Money -> [Advance] -> Projection -> Abs Money
amount credit advances health =
  absolute $ min
    (creditRemaining credit advances)
    (nearest50 $ abs $ expenses health - available health)
  where
    nearest50 = nearestUp (Money.fromFloat 50)


nearestUp :: Money -> Money -> Money
nearestUp amount m =
  fromIntegral (ceiling (Money.toFloat m / Money.toFloat amount)) * (Money.toFloat amount)
    & Money.fromFloat



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



isEnough :: Money -> Money -> [Advance] -> Bool
isEnough amount credit advances =
  amount <= creditRemaining credit advances


creditRemaining :: Money -> [Advance] -> Money
creditRemaining credit advances =
  let creditUsed = sum (map Advances.amount advances)
  in credit - creditUsed
