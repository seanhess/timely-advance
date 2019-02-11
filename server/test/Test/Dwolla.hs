{-# LANGUAGE OverloadedStrings #-}
module Test.Dwolla where

import           Test.Tasty.HUnit
import           Test.Tasty.Monad

import           Network.Dwolla            as Dwolla
import           Servant.Client            (BaseUrl(..), Scheme(..))

tests :: Tests ()
tests = do
    group "funding-sources" testFundingSources



testFundingSources :: Tests ()
testFundingSources = do
    group "links" $ do
      let base = BaseUrl Http "test.example.com" 80 ""
      test "should create correct links" $ do
        Dwolla.fundingSource base (Id "asdf-1234") @?= Resource "http://test.example.com/funding-sources/asdf-1234"
