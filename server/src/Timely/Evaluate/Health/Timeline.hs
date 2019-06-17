{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeApplications      #-}
module Timely.Evaluate.Health.Timeline where

import Data.Function                      ((&))
import Data.List                          as List (mapAccumL, filter, sum, concatMap, sortOn)
import Data.Model.Money                   (Money)
import Data.Number.Abs                    (Abs (value), absolute)
import Data.Time.Calendar                 (Day)
import Timely.Evaluate.Health.Budget      as Budget (Budget (..), Scheduled (..))
import Timely.Evaluate.Health.Daily       as Daily (Daily (..), DailyBalance (..))
import Timely.Evaluate.Health.Transaction (Expense)
import Timely.Evaluate.Schedule           as Schedule (until)











-- | Calculate the minimum balance for the timeline
minimumBalance :: Money -> [DailyBalance] -> Money
minimumBalance bal [] = bal
minimumBalance bal es = minimum $ bal : fmap balance es



-- | Calculate the total discretionary spending
totalSpending :: [Daily] -> Abs Money
totalSpending =
  absolute . List.sum . fmap (value.spending)


billsDue :: [Daily] -> [Scheduled Expense]
billsDue = List.concatMap scheduledBills
  where
    scheduledBills Daily {date, bills} =
      fmap (scheduledBill date) bills

    scheduledBill date bill =
      Scheduled bill date



-- | Return the daily balance projection
-- pass in the paycheck date (end of projection) = Schedule.next (schedule paycheck) now
timeline :: Day -> Day -> Abs Money -> [Budget Expense] -> [Daily]
timeline now end dailySpending bills =
  let scheds = schedulesAll now end bills
  in  fmap (dailyFromScheduled scheds dailySpending) [now..end]



dailyBalances :: Money -> [Daily] -> [DailyBalance]
dailyBalances balance =
  snd . List.mapAccumL addDaily balance
  where
    addDaily :: Money -> Daily -> (Money, DailyBalance)
    addDaily bal daily =
      let bal' = bal - value (dailyTotal daily)
      in (bal', DailyBalance daily bal')



dailyFromScheduled :: [Scheduled Expense] -> Abs Money -> Day -> Daily
dailyFromScheduled scheds dailySpending d = Daily
  { date = d
  , spending = dailySpending
  , bills = fmap Budget.budget $ List.filter (\s -> Budget.date s == d) scheds
  }


-- this should have been absolute
dailyTotal :: Daily -> Abs Money
dailyTotal Daily {spending, bills} =
  absolute $ value spending + (List.sum $ fmap (value . Budget.amount) bills)



schedulesAll :: Day -> Day -> [Budget a] -> [Scheduled a]
schedulesAll start end bs =
  List.concatMap (schedules start end) bs & List.sortOn Budget.date


schedules :: Day -> Day -> Budget a -> [Scheduled a]
schedules start end budget =
  Schedule.until (<= end) (schedule budget) start
    & fmap (Scheduled budget)
