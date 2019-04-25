{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeApplications      #-}
module Timely.Evaluate.Health.Projection where

import           Data.Aeson                         (ToJSON)
import           Data.Function                      ((&))
import qualified Data.List                          as List
import           Data.Model.Money                   (Money)
import           Data.Number.Abs                    (Abs (value))
import           Data.Time.Calendar                 (Day)
import           Data.Time.Calendar                 (addGregorianMonthsClip)
import           GHC.Generics                       (Generic)
import           Timely.Evaluate.Health.Budget      (Budget (..))
import           Timely.Evaluate.Health.Event       (Event (Event))
import qualified Timely.Evaluate.Health.Event       as Event
import           Timely.Evaluate.Health.Transaction (Any, Expense, Income, Transaction (..))
import qualified Timely.Evaluate.Health.Transaction as Trans
import qualified Timely.Evaluate.Schedule           as Schedule



data Projection = Projection
  { balance :: Money
  , lowest  :: Money
  , events  :: [Event]
  } deriving (Show, Eq, Generic)

instance ToJSON Projection





projection :: Day -> Money -> [Budget Income] -> [Budget Expense] -> Projection
projection now balance pays bills =
  let evs = allEvents balance $ allTransactions now pays bills
  in Projection
      { balance = balance
      , lowest = lowestBalance balance evs
      , events = evs
      }




lowestBalance :: Money -> [Event] -> Money
lowestBalance bal [] = bal
lowestBalance bal es = minimum $ bal : List.map Event.balance es




allTransactions :: Day -> [Budget Income] -> [Budget Expense] -> [Transaction Any]
allTransactions now pays bills =
  let creds = List.concatMap (credits now) pays
      debs = List.concatMap (debits now) bills
  in List.sortOn date $ debs ++ creds


allEvents :: Money -> [Transaction Any] -> [Event]
allEvents balance =
  snd . List.mapAccumL addEvent balance
  where
    addEvent :: Money -> Transaction Any -> (Money, Event)
    addEvent b t =
      let b' = b + (Trans.amount t)
      in (b', Event t b')


credits :: Day -> Budget Income -> [Transaction Any]
credits = trans value


debits :: Day -> Budget Expense -> [Transaction Any]
debits = trans (negate . value)


trans :: (Abs Money -> Money) -> Day -> Budget a -> [Transaction Any]
trans f now budget@Budget{name, amount} =
  dates now budget
    & List.map (Trans.any name (f amount))


dates :: Day -> Budget a -> [Day]
dates now Budget{schedule} =
  let end = addGregorianMonthsClip 1 now
  in Schedule.until (<= end) schedule now
