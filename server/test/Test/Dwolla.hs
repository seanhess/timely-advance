{-# LANGUAGE OverloadedStrings #-}
module Test.Dwolla where

import           Test.Tasty.HUnit
import           Test.Tasty.Monad

import           Network.Dwolla         as Dwolla
import           Servant.Client         (BaseUrl (..), Scheme (..))

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (asks)
import           Servant.Client         (runClientM, mkClientEnv)
import           Timely.Api.AppM        as AppM
import           Timely.Config          (dwollaClientId, dwollaSecret, dwollaBaseUrl)

tests :: Tests ()
tests = do
    group "funding-sources" testFundingSources



testFundingSources :: Tests ()
testFundingSources = do
    group "links" $ do
      let base = BaseUrl Http "test.example.com" 80 ""
      test "should create correct links" $ do
        Dwolla.fundingSource base (Id "asdf-1234") @?= Resource "http://test.example.com/funding-sources/asdf-1234"



integration :: IO ()
integration = do
  state <- AppM.loadState
  AppM.runIO state $ do
    client <- asks (dwollaClientId . env)
    secret <- asks (dwollaSecret . env)
    mgr <- asks manager
    url <- asks (dwollaBaseUrl . env)
    let clientEnv = mkClientEnv mgr url

    liftIO $ print ("HI", client, secret)

    res <- liftIO $ runClientM (Dwolla.authenticate client secret) clientEnv

    liftIO $ print ("DONE", res)




