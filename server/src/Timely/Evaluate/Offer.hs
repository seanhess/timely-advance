module Timely.Evaluate.Offer where


import           Data.Function    ((&))
import qualified Data.List        as List
import qualified Data.Maybe       as Maybe
import           Data.Model.Money as Money
import           Data.Number.Abs  (Abs, absolute)
import           Data.Time.Clock  (NominalDiffTime, UTCTime (..))
import qualified Data.Time.Clock  as Time
import           Timely.Advances  (Advance)
import qualified Timely.Advances  as Advances

-- If they have any active advances, the trigger amount changes to 250.00
-- If they have any recent advances, don't advance
-- If they have any unclaimed offers at all, don't advance


type LowestBalance = Money


isNeeded :: Maybe Advance -> [Advance] -> LowestBalance -> UTCTime -> Bool
isNeeded offer active lowest today =
  Maybe.isJust $ check (Money.fromFloat 0) offer active lowest today


check :: Money -> Maybe Advance -> [Advance] -> LowestBalance -> UTCTime -> Maybe (Abs Money)
check credit offer active lowest today
  | isAnyRecent today $ advanceTimes offer active = Nothing
  | Maybe.isJust offer = Nothing
  | lowest >= 0 = Nothing
  | otherwise = Just $ amount credit active lowest





amount :: Money -> [Advance] -> LowestBalance -> Abs Money
amount credit advances lowest =
  absolute $ min
    (creditRemaining credit advances)
    (nearest50 $ abs $ lowest)
  where
    nearest50 = nearestUp (Money.fromFloat 50)



nearestUp :: Money -> Money -> Money
nearestUp amount m =
  fromIntegral (ceiling (Money.toFloat m / Money.toFloat amount)) * (Money.toFloat amount)
    & Money.fromFloat



advanceTimes :: Maybe Advance -> [Advance] -> [UTCTime]
advanceTimes offer active =
  Maybe.catMaybes $ (fmap Advances.offered offer : map Advances.activated active)


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
