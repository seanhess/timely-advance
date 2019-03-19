{-# LANGUAGE OverloadedStrings #-}
module Test.Notify where

import           Data.Model.Guid  (Guid)
import           Servant.Client   (BaseUrl (..), Scheme (..), parseBaseUrl, showBaseUrl)
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Timely.Advances  (Advance)
import           Timely.Notify    as Notify

tests :: Tests ()
tests = do
    group "url" $ do
      let advanceId = "111" :: Guid Advance
      let accountId = "000"
      let message = Message advanceId Advance "hello"
      let base = BaseUrl Https "timelyadvance.com" 443 "app"

      test "should match expected url" $ do
        Notify.url base accountId message @?= ("https://timelyadvance.com/app/#/accounts/acc-000/advances/adv-111")


      test "baseurl ignores slash" $ do
        let Right one = parseBaseUrl "https://timelyadvance.com/app"
        let Right two = parseBaseUrl "https://timelyadvance.com/app/"
        showBaseUrl one @?= showBaseUrl two
