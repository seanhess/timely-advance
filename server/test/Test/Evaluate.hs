{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate where

import           Data.Model.Id            (Id (..))
import           Data.Model.Money         (Money (..))
import qualified Data.Model.Money         as Money
import           Data.Number.Abs          (absolute)
import           Data.Time.Calendar       (Day)
import qualified Data.Time.Calendar       as Day
import qualified Data.Time.Clock          as Time
import           Test.Dates               (parseDay, parseTime)
import qualified Test.Evaluate.Health     as Health
import qualified Test.Evaluate.History    as History
import qualified Test.Evaluate.Schedule   as Schedule
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Advances          as Advance (Advance (..))
import           Timely.Evaluate.Offer    (Projection (..))
import qualified Timely.Evaluate.Offer    as Offer
import qualified Timely.Evaluate.Offer    as Credit
import qualified Timely.Evaluate.Schedule as Schedule


tests :: Tests ()
tests = do
    group "offer" testOffer
    group "paydate" testSchedule
    group "credit" testCredit
    group "history" History.tests
    group "schedule" Schedule.tests
    group "health" Health.tests







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







testOffer :: Tests ()
testOffer = do
    group "isRecent" $ do

      test "1m ago is recent" $ do
        now  <- Time.getCurrentTime
        Offer.isRecentTime now (ago1m now) @?= True

      test "1m under interval is recent" $ do
        now  <- Time.getCurrentTime
        Offer.isRecentTime now (agoIntUnder now) @?= True

      test "1m over interval is not recent" $ do
        now  <- Time.getCurrentTime
        Offer.isRecentTime now (agoIntOver now) @?= False


    group "isAnyRecent" $ do
      test "should find one recent" $ do
        now  <- Time.getCurrentTime
        Offer.isAnyRecent now [agoRecent now, agoOld now] @?= True

      test "empty is not recent" $ do
        now  <- Time.getCurrentTime
        Offer.isAnyRecent now [] @?= False

      test "should all be old " $ do
        now  <- Time.getCurrentTime


        Offer.isAnyRecent now [agoOld now ] @?= False



    group "isNeeded" $ do

      test "should not advance with recent offer" $ do
        let health = Projection {expenses = Money 20000, available = Money 11000}
            offer = parseTime "2019-02-01T20:02:46"
            now   = parseTime "2019-02-01T20:04:10"
        Offer.isNeeded (Just $ advance offer) [] health now @?= False

      test "should not advance with old offer" $ do
        let health = Projection {expenses = Money 20000, available = Money 11000}
            now   = parseTime "2019-02-04T20:04:10"
            offer = parseTime "2019-02-01T20:04:10"
        Offer.isNeeded (Just $ advance offer) [] health now @?= False

      test "should advance with no offers" $ do
        let health = Projection {expenses = Money 20000, available = Money 11000}
            now = parseTime "2019-02-01T20:04:10"
        Offer.isNeeded Nothing [] health now @?= True

      test "should not advance if sufficient funds" $ do
        let health = Projection {expenses = Money 20000, available = Money 110000000}
            now = parseTime "2019-02-01T20:04:10"
        Offer.isNeeded Nothing [] health now @?= False



    group "nearestUp" $ do
      let nearest50 = Offer.nearestUp (Money.fromFloat 50)

      test "multiple of 50" $ do
        nearest50 (Money.fromFloat 100) @?= Money.fromFloat 100

      test "rounds up" $ do
        nearest50 (Money.fromFloat 90) @?= Money.fromFloat 100

      test "zero" $ do
        nearest50 (Money.fromFloat 0) @?= Money.fromFloat 0

      test "rounds up again" $ do
        nearest50 (Money.fromFloat 34) @?= Money.fromFloat 50


    group "amount" $ do
      test "amount of difference needed" $ do
        let health = Projection { expenses = Money.fromFloat 200, available = Money.fromFloat 100 }
            credit = Money.fromFloat 10000000
        Offer.amount credit [] health @?= absolute (Money.fromFloat 100)

      test "offers in multiples of 50" $ do
        let health = Projection { expenses = Money.fromFloat 210, available = Money.fromFloat 15 }
            credit = Money.fromFloat 10000000
        Offer.amount credit [] health @?= absolute (Money.fromFloat 200)

      test "capped at credit" $ do
        let health = Projection { expenses = Money.fromFloat 200, available = Money.fromFloat 100 }
            credit = Money.fromFloat 20
        Offer.amount credit [] health @?= absolute (Money.fromFloat 20)



  where
    ago1m       = Time.addUTCTime (-60)
    agoIntUnder = Time.addUTCTime (-(Time.nominalDay - 60))
    agoIntOver  = Time.addUTCTime (-(Time.nominalDay + 60))
    agoOld      = Time.addUTCTime (-(Time.nominalDay * 2))
    agoRecent   = ago1m

    advance time = Advance {advanceId = "34209d46-efd2-4675-aa8e-8564d9ab65b6", accountId = "758547fd-74a8-48c3-8fd6-390b515027a5", amount = Money 20000, offer = Money 20000, due = parseTime "2019-02-04", offered = time, activated = Nothing, collected = Nothing, transferId = Id ""}




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
