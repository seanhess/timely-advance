{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeApplications      #-}
module Timely.Evaluate.Health.Timeline where

import Data.Function                      ((&))
import Data.List                          as List (mapAccumL, filter, sum, concatMap, sortOn, map)
import Data.Model.Money                   (Money)
import Data.Number.Abs                    (Abs (value), absolute)
import Data.Time.Calendar                 (Day)
import Timely.Evaluate.Health.Budget      as Budget (Budget, BudgetInfo(..), budget)
import Timely.Evaluate.Health.Scheduled   as Scheduled (Scheduled (..))
import Timely.Evaluate.Health.Daily       as Daily (Daily (..), DailyBalance (..))
import Timely.Evaluate.Health.Transaction (Expense)
import Timely.Evaluate.Schedule           as Schedule (until, Schedule)











-- | Calculate the minimum balance for the timeline
minimumBalance :: Money -> [DailyBalance] -> Money
minimumBalance bal [] = bal
minimumBalance bal es = minimum $ bal : fmap balance es



-- | Calculate the total discretionary spending
totalSpending :: [Daily] -> Abs Money
totalSpending =
  absolute . List.sum . fmap (value.spending)


billsDue :: [Daily] -> [Scheduled (Budget Expense)]
billsDue = List.concatMap scheduledBills
  where
    scheduledBills Daily {date, bills} =
      List.map (Scheduled date) bills



-- | Return the daily balance projection
-- pass in the paycheck date (end of projection) = Schedule.next (schedule paycheck) now
timeline :: Day -> Day -> Abs Money -> [Budget Expense] -> [Daily]
timeline now end dailySpending bills =
  let scheds = schedulesAll (Budget.schedule . Budget.budget) now end bills
  in  fmap (dailyFromScheduled scheds dailySpending) [now..end]



dailyBalances :: Money -> [Daily] -> [DailyBalance]
dailyBalances balance =
  snd . List.mapAccumL addDaily balance
  where
    addDaily :: Money -> Daily -> (Money, DailyBalance)
    addDaily bal daily =
      let bal' = bal - value (dailyTotal daily)
      in (bal', DailyBalance daily bal')



dailyFromScheduled :: [Scheduled (Budget Expense)] -> Abs Money -> Day -> Daily
dailyFromScheduled scheds dailySpending d = Daily
  { date = d
  , spending = dailySpending
  , bills = fmap Scheduled.item $ List.filter (\s -> Scheduled.date s == d) scheds
  }


-- this should have been absolute
dailyTotal :: Daily -> Abs Money
dailyTotal Daily {spending, bills} =
  absolute $ value spending + (List.sum $ fmap (value . Budget.amount . Budget.budget) bills)



schedulesAll :: (a -> Schedule) -> Day -> Day -> [a] -> [Scheduled a]
schedulesAll schedule start end as =
  List.concatMap (schedules schedule start end) as & List.sortOn Scheduled.date


schedules :: (a -> Schedule) -> Day -> Day -> a -> [Scheduled a]
schedules schedule start end item =
  Schedule.until (<= end) (schedule item) start
    & List.map (\d -> Scheduled d item)
