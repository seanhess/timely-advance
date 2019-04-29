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




allTransactions :: Day -> [Budget Income] -> [Budget Expense] -> [(Budget Any, Transaction Any)]
allTransactions now pays bills =
  let creds = List.concatMap (budgetTransactions (credits now)) pays
      debs  = List.concatMap (budgetTransactions (debits now)) bills
  in List.sortOn (date . snd) $ debs ++ creds


budgetTransactions :: (Budget a -> [Transaction Any]) -> Budget a -> [(Budget Any, Transaction Any)]
budgetTransactions findTransactions b@(Budget n s a) =
  zip (repeat (Budget n s a)) (findTransactions b)


allEvents :: Money -> [(Budget Any, Transaction Any)] -> [Event]
allEvents balance =
  snd . List.mapAccumL addEvent balance
  where
    addEvent :: Money -> (Budget Any, Transaction Any) -> (Money, Event)
    addEvent bal (bgt, trans) =
      let bal' = bal + (Trans.amount trans)
      in (bal', Event trans bal' bgt)


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
