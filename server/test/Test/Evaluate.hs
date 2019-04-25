{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate where

import           Data.Model.Id                     (Id (..))
import           Data.Model.Money                  (Money (..))
import qualified Data.Model.Money                  as Money
import           Data.Time.Calendar                (Day)
import qualified Data.Time.Calendar                as Day
import           Test.Dates                        (parseDay, parseTime)
import qualified Test.Evaluate.History             as History
import qualified Test.Evaluate.Needed              as Needed
import qualified Test.Evaluate.Schedule            as Schedule
import qualified Test.Evaluate.Projection          as Projection
import qualified Test.Evaluate.Offer               as Offer
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Advances                   as Advance (Advance (..))
import qualified Timely.Evaluate.Offer             as Credit
import qualified Timely.Evaluate.Schedule          as Schedule


tests :: Tests ()
tests = do
    group "offer" Offer.tests
    group "paydate" testSchedule
    group "credit" testCredit
    group "history" History.tests
    group "schedule" Schedule.tests
    group "needed" Needed.tests
    group "projection" Projection.tests







testSchedule :: Tests ()
testSchedule = do
    let monday = parseDay "2019-02-04" :: Day
    group "dayOfWeek" $ do
      test "should be monday" $ do
        Schedule.dayOfWeek monday @?= Schedule.Monday

      test "should be tuesday" $ do
        Schedule.dayOfWeek (Day.addDays 1 monday) @?= Schedule.Tuesday

    group "nextWeekday" $ do
      test "next monday is 7 days away" $ do
        Schedule.nextWeekday Schedule.Monday monday @?= parseDay "2019-02-11"

      test "next tuesday is 1 day away" $ do
        Schedule.nextWeekday Schedule.Tuesday monday @?= parseDay "2019-02-05"

      test "next sunday is 6 days away" $ do
        Schedule.nextWeekday Schedule.Sunday monday @?= parseDay "2019-02-10"



testCredit :: Tests ()
testCredit = do
    group "isEnough" $ do
      let m200 = Money.fromFloat 200.00

      test "should be enough if less than limit" $ do
        Credit.isEnough (Money.fromFloat 100.00) m200 [] @?= True

      test "should NOT be enough if MORE than limit" $ do
        Credit.isEnough (Money.fromFloat 250.00) m200 [] @?= False

      test "should be enough if equal limit" $ do
        Credit.isEnough (Money.fromFloat 200.00) m200 [] @?= True

      test "should not be enough if advances reduce credit" $ do
        let advance = sample { amount = Money.fromFloat 100.00 }
        Credit.isEnough (Money.fromFloat 150.00) m200 [advance] @?= False

      test "should be enough if advances leave a little credit" $ do
        let advance = sample { amount = Money.fromFloat 50.00 }
        Credit.isEnough (Money.fromFloat 150.00) m200 [advance] @?= True



sample = Advance {advanceId = "34209d46-efd2-4675-aa8e-8564d9ab65b6", Advance.accountId = "758547fd-74a8-48c3-8fd6-390b515027a5", amount = Money 20000, offer = Money 20000, due = parseDay "2019-02-04", offered = parseTime "2019-02-01T20:02:46", activated = Nothing, collected = Nothing, Advance.transferId = Id "transfer" }
