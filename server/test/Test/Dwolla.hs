{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Dwolla where

import           Test.Dates              (parseDay)
import           Test.Tasty.HUnit
import           Test.Tasty.Monad

import           Network.Dwolla          as Dwolla
import           Servant.Client          (BaseUrl (..), Scheme (..))

import           Control.Monad.Catch     (throwM)
import           Control.Monad.Config    (MonadConfig (..))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (asks)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Servant
import           Servant.Client          (mkClientEnv, runClientM)
import           Test.RandomStrings      (onlyAlphaNum, randomASCII, randomString)
import           Timely.Api.AppM         as AppM
import           Timely.Config           (dwollaAuthBaseUrl, dwollaBaseUrl)

import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

tests :: Tests ()
tests = do
    group "funding-sources" testFundingSources
    group "authorization" testAuthorization



testFundingSources :: Tests ()
testFundingSources = do
    group "links" $ do
      let base = BaseUrl Http "test.example.com" 80 ""

      test "should create correct links" $ do
        Dwolla.fundingSource base (Id "asdf-1234") @?= Resource "http://test.example.com/funding-sources/asdf-1234"


testAuthorization :: Tests ()
testAuthorization = do
  group "basic auth" $ do
    test "basic auth header" $ do
      Servant.toUrlPiece (Dwolla.Credentials (Id "user") (Id "pass")) @?= "Basic dXNlcjpwYXNz"

    test "auth token header" $ do
      Servant.toUrlPiece (Dwolla.AuthToken "woot") @?= "Bearer woot"




integration :: IO ()
integration = do
  state <- AppM.loadState
  AppM.runIO state $ do
    creds <- config
    mgr <- asks manager
    -- mgr <- debugManager
    url <- asks (dwollaAuthBaseUrl . env)
    let clientEnv = mkClientEnv mgr url
    res <- liftIO $ runClientM (Dwolla.authenticate creds) clientEnv
    tok <- result res
    liftIO $ print ("DONE", tok)
    integrationAuth tok


integrationAuth tok = do
    liftIO $ putStrLn "-- Create Customer ------------"
    mgr <- asks manager
    url <- asks (dwollaBaseUrl . env)
    let clientEnv = mkClientEnv mgr url
    r <- randomName
    res <- liftIO $ runClientM (Dwolla.createCustomer tok (customer r)) clientEnv
    liftIO $ print ("Created", res)
    id <- result res

    pure ()




result res = do
    case res of
      Left e -> throwM e
      Right v -> pure v


debugManager :: AppM HTTP.Manager
debugManager =
  liftIO $ HTTP.newManager HTTP.tlsManagerSettings { HTTP.managerModifyRequest = modifyRequest }
  where
    modifyRequest req = do
      print req
      pure req


randomName :: MonadIO m => m Text
randomName = do
  s <- liftIO $ randomString (onlyAlphaNum randomASCII) 10
  pure $ cs s


customer rand = Customer
  { firstName   = "Test"
  , lastName    = rand
  , email       = rand <> "@example.com"
  , ipAddress   = Nothing
  , type_       = Static
  , address1    = Address "1234 W Big Street"
  , address2    = Nothing
  , city        = "Salt Lake City"
  , state       = "UT"
  , postalCode  = "84106"
  , dateOfBirth = parseDay "1982-08-12"
  , ssn         = Last4SSN "1234"
  , phone       = Nothing
  }
