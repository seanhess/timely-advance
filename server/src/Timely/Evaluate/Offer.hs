module Timely.Evaluate.Offer where


import qualified Data.List              as List
import qualified Data.Maybe             as Maybe
import           Data.Model.Money       as Money
import           Data.Number.Abs        (Abs (value))
import           Data.Time.Clock        (NominalDiffTime, UTCTime (..))
import qualified Data.Time.Clock        as Time
import           Timely.Advances        (Advance (..))
import           Timely.Evaluate.Health (AccountHealth (..))

-- If they have any active advances, the trigger amount changes to 250.00
-- If they have any recent advances, don't advance
-- If they have any unclaimed offers at all, don't advance

isNeeded :: Maybe Advance -> [Advance] -> AccountHealth -> UTCTime -> Bool
isNeeded offer active health today
  | isAnyRecent today $ advanceTimes offer active = False
  | Maybe.isJust offer = False
  | otherwise = balance health < value (budgeted health)


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


amount :: Money
amount = Money.fromFloat 200.00
