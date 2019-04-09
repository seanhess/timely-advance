{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate where

import           Data.Model.Id            (Id (..))
import           Data.Model.Money         (Money (..))
import           Data.Time.Calendar       (Day)
import qualified Data.Time.Calendar       as Day
import qualified Data.Time.Clock          as Time
import           Test.Dates               (parseDay, parseTime)
import qualified Test.Evaluate.Health     as Health
import qualified Test.Evaluate.History    as History
import qualified Test.Evaluate.Schedule   as Schedule
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Advances          (Advance (..))
import qualified Timely.Evaluate.Offer    as Offer
import qualified Timely.Evaluate.Schedule as Schedule
import           Timely.Evaluate.Types    (Projection (..))

tests :: Tests ()
tests = do
    group "offer" testOffer
    group "paydate" testSchedule
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


    group "triggerAmount" $ do
      test "should be higher if there are no advances" $ do
        Offer.triggerAmount [] > Offer.triggerAmount [undefined] @? "greater"



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



  where
    ago1m       = Time.addUTCTime (-60)
    agoIntUnder = Time.addUTCTime (-(Time.nominalDay - 60))
    agoIntOver  = Time.addUTCTime (-(Time.nominalDay + 60))
    agoOld      = Time.addUTCTime (-(Time.nominalDay * 2))
    agoRecent   = ago1m

    advance time = Advance {advanceId = "34209d46-efd2-4675-aa8e-8564d9ab65b6", accountId = "758547fd-74a8-48c3-8fd6-390b515027a5", amount = Money 20000, offer = Money 20000, due = parseTime "2019-02-04", offered = time, activated = Nothing, collected = Nothing, transferId = Id ""}
