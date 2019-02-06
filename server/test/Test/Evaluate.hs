{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate where

import qualified Data.Maybe            as Maybe
import           Data.Time.Calendar    (Day)
import qualified Data.Time.Calendar    as Day
import qualified Data.Time.Clock       as Time
import qualified Data.Time.Format      as Time
import           Test.Tasty.HUnit
import           Test.Tasty.Monad

import           Timely.Advances       (Advance (..))
import qualified Timely.Evaluate.Offer as Offer
import qualified Timely.Evaluate.Paydate as Paydate
import           Timely.Evaluate.Types (Projection (..))
import           Timely.Types.Money    (Money (..))

tests :: Tests ()
tests = do
    group "offer" testOffer
    group "paydate" testPaydate







testPaydate :: Tests ()
testPaydate = do
    let monday = parseDay "2019-02-04" :: Day
    group "toDayOfWeek" $ do
      test "should be monday" $ do
        Paydate.toDayOfWeek monday @?= Paydate.mon

      test "should be tuesday" $ do
        Paydate.toDayOfWeek (Day.addDays 1 monday) @?= Paydate.tue

    group "nextWeekday" $ do
      test "next monday is 7 days away" $ do
        Paydate.nextWeekday Paydate.mon monday @?= parseDay "2019-02-11"

      test "next tuesday is 1 day away" $ do
        Paydate.nextWeekday Paydate.tue monday @?= parseDay "2019-02-05"

      test "next sunday is 6 days away" $ do
        Paydate.nextWeekday Paydate.sun monday @?= parseDay "2019-02-10"







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
        let advance = Advance {advanceId = "34209d46-efd2-4675-aa8e-8564d9ab65b6", accountId = "758547fd-74a8-48c3-8fd6-390b515027a5", amount = Money 20000, offer = Money 20000, due = parseTime "2019-02-04", offered = parseTime "2019-02-01T20:02:46", activated = Nothing, collected = Nothing}
            health = Projection {expenses = Money 20000, available = Money 11000}
            now = parseTime "2019-02-01T20:04:10"
        Offer.isNeeded (Just advance) [] health now @?= False

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



parseTime :: Time.ParseTime a => String -> a
parseTime a = Maybe.fromMaybe (error a) $ Time.parseTimeM True Time.defaultTimeLocale (Time.iso8601DateFormat (Just "%H:%M:%S")) a

parseDay :: Time.ParseTime a => String -> a
parseDay a = Maybe.fromMaybe (error a) $ Time.parseTimeM True Time.defaultTimeLocale (Time.iso8601DateFormat Nothing) a

