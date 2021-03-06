{-# LANGUAGE OverloadedStrings #-}
module Test.Advances where

import           Data.Model.Id           (Id (..))
import           Data.Model.Money        as Money
import           Data.Model.Valid        as Valid
import qualified Data.Time.Clock         as Time
import           Test.Dates              (parseDay, parseTime)
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Accounts.Types   as Account (Account (..))
import           Timely.Advances         as Advance (Advance (..))
import qualified Timely.Advances.Collect as Collect
import qualified Timely.Advances.Store   as Store

tests :: Tests ()
tests = do
    group "advances" testMain
    group "collect" testCollect





testMain :: Tests ()
testMain = do
    group "findOffer" $ do
      test "should find single offer" $ do
        now <- Time.getCurrentTime
        let a = sample { offered = now, activated = Nothing, collected = Nothing }
        Store.findOffer [a] @?= Just a

      test "should find recent offer" $ do
        (t1, t2) <- times
        let a1 = sample { offered = t1, activated = Nothing, collected = Nothing }
        let a2 = sample { offered = t2, activated = Nothing, collected = Nothing }
        Store.findOffer [a1, a2] @?= Just a2

      test "should expire older offer" $ do
        (t1, t2) <- times
        let a1 = sample { offered = t1, activated = Nothing, collected = Nothing }
        let a2 = sample { offered = t2, activated = Just t2, collected = Nothing }
        Store.findOffer [a1, a2] @?= Nothing

      test "should not find offer among activated" $ do
        (t1, t2) <- times
        let a1 = sample { offered = t1, activated = Just t1, collected = Nothing }
        let a2 = sample { offered = t2, activated = Just t2, collected = Nothing }
        Store.findOffer [a1, a2] @?= Nothing

      test "should find newer offer than activated" $ do
        (t1, t2) <- times
        let a1 = sample { offered = t1, activated = Just t1, collected = Nothing }
        let a2 = sample { offered = t2, activated = Nothing, collected = Nothing }
        Store.findOffer [a1, a2] @?= Just a2



testCollect :: Tests ()
testCollect = do
    group "currentlyDue" $ do
      test "12pm isn't due yet - previous day" $ do
        Collect.currentlyDue (parseTime "2019-02-02T12:00:00") @?= (parseDay "2019-02-01")

      test "11pm isn't due yet - previous day" $ do
        Collect.currentlyDue (parseTime "2019-02-02T23:00:00") @?= (parseDay "2019-02-01")

      test "12am is collection time, now the day becomes due" $ do
        Collect.currentlyDue (parseTime "2019-02-03T00:00:00") @?= (parseDay "2019-02-02")

      test "1am is past collection time" $ do
        Collect.currentlyDue (parseTime "2019-02-03T01:00:00") @?= (parseDay "2019-02-02")



times = do
  t1 <- Time.getCurrentTime
  let t2 = Time.addUTCTime (60) t1
  pure (t1, t2)


sample = Advance {advanceId = "34209d46-efd2-4675-aa8e-8564d9ab65b6", Advance.accountId = "758547fd-74a8-48c3-8fd6-390b515027a5", amount = Money 20000, offer = Money 20000, due = parseDay "2019-02-04", offered = parseTime "2019-02-01T20:02:46", activated = Nothing, collected = Nothing, Advance.transferId = Id "transfer" }

sampleAccount = Account { Account.accountId = "acc09d46-efd2-4675-aa8e-8564d9ab65b6", phone = Valid "8012223333", bankToken = undefined, bankItemId = undefined, Account.transferId = Id "transfer", created = parseTime "2019-02-01T20:02:46"}

-- parseTime :: Time.ParseTime a => String -> a
-- parseTime a = Maybe.fromMaybe (error a) $ Time.parseTimeM True Time.defaultTimeLocale (Time.iso8601DateFormat (Just "%H:%M:%S")) a

-- parseDay :: Time.ParseTime a => String -> a
-- parseDay a = Maybe.fromMaybe (error a) $ Time.parseTimeM True Time.defaultTimeLocale (Time.iso8601DateFormat Nothing) a
