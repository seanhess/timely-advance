{-# LANGUAGE OverloadedStrings #-}
module Test.Notify where

import           Servant.Client   (BaseUrl (..), Scheme(..), showBaseUrl, parseBaseUrl)
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Notify    as Notify

tests :: Tests ()
tests = do
    group "url" $ do
      let advanceId = "34209d46-efd2-4675-aa8e-8564d9ab1111"
      let accountId = "34209d46-efd2-4675-aa8e-8564d9ab0000"
      let message = Message advanceId Advance "hello"
      let base = BaseUrl Https "timelyadvance.com" 443 "app"

      test "should match expected url" $ do
        Notify.url base accountId message @?= ("https://timelyadvance.com/app/#/accounts/34209d46-efd2-4675-aa8e-8564d9ab0000/advances/34209d46-efd2-4675-aa8e-8564d9ab1111")


      test "baseurl ignores slash" $ do
        let Right one = parseBaseUrl "https://timelyadvance.com/app"
        let Right two = parseBaseUrl "https://timelyadvance.com/app/"
        showBaseUrl one @?= showBaseUrl two
