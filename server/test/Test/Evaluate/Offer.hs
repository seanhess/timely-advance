{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate.Offer where

import           Data.Model.Id            (Id (..))
import           Data.Model.Money         (Money (..))
import qualified Data.Model.Money         as Money
import           Data.Number.Abs          (absolute)
import qualified Data.Time.Clock          as Time
import           Test.Dates               (parseTime)
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Advances          as Advance (Advance (..))
import qualified Timely.Evaluate.Offer    as Offer


tests :: Tests ()
tests = do
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
    let low  = Money.fromFloat (-100)
    let high = Money.fromFloat 100

    test "should not advance with recent offer" $ do
      let offer = parseTime "2019-02-01T20:02:46"
          now   = parseTime "2019-02-01T20:04:10"
      Offer.isNeeded (Just $ advance offer) [] low now @?= False

    test "should not advance with old offer" $ do
      let now   = parseTime "2019-02-04T20:04:10"
          offer = parseTime "2019-02-01T20:04:10"
      Offer.isNeeded (Just $ advance offer) [] low now @?= False

    test "should advance with no offers" $ do
      let now = parseTime "2019-02-01T20:04:10"
      Offer.isNeeded Nothing [] low now @?= True

    test "should not advance if sufficient funds" $ do
      let now = parseTime "2019-02-01T20:04:10"
      Offer.isNeeded Nothing [] high now @?= False



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
    let low = Money.fromFloat ( -100 )

    test "amount of difference needed" $ do
      let credit = Money.fromFloat 10000000
      Offer.amount credit [] low @?= absolute low

    test "offers in multiples of 50" $ do
      let credit = Money.fromFloat 10000000
      Offer.amount credit [] (Money.fromFloat (-195)) @?= absolute (Money.fromFloat 200)

    test "capped at credit" $ do
      let credit = Money.fromFloat 20
      Offer.amount credit [] low @?= absolute credit

  where
    ago1m       = Time.addUTCTime (-60)
    agoIntUnder = Time.addUTCTime (-(Time.nominalDay - 60))
    agoIntOver  = Time.addUTCTime (-(Time.nominalDay + 60))
    agoOld      = Time.addUTCTime (-(Time.nominalDay * 2))
    agoRecent   = ago1m

    advance time = Advance {advanceId = "34209d46-efd2-4675-aa8e-8564d9ab65b6", accountId = "758547fd-74a8-48c3-8fd6-390b515027a5", amount = Money 20000, offer = Money 20000, due = parseTime "2019-02-04", offered = time, activated = Nothing, collected = Nothing, transferId = Id ""}
