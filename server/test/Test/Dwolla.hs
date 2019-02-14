{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Dwolla where

import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Servant.Client          (BaseUrl (..), Scheme (..))
import qualified Servant
import           Network.Dwolla          as Dwolla

-- import qualified Network.Plaid.Dwolla    as Plaid
-- import           Network.Plaid           as Plaid
-- import           Data.Model.Id           (Token)
-- import           Test.Dates              (parseDay)
-- import           Control.Monad.Catch     (throwM)
-- import           Control.Monad.Config    (MonadConfig (..), configs)
-- import           Control.Monad.IO.Class  (MonadIO, liftIO)
-- import           Control.Monad.Reader    (asks)
-- import           Data.String.Conversions (cs)
-- import           Data.Text               (Text)
-- import           Servant.Client          (mkClientEnv, runClientM)
-- import           Test.RandomStrings      (onlyAlphaNum, randomASCII, randomString)
-- import qualified Timely.Bank             as Bank
-- import           Timely.Config           (dwollaAuthBaseUrl, dwollaBaseUrl)
-- import           Timely.Worker.WorkerM   as WorkerM
-- import qualified Network.HTTP.Client     as HTTP
-- import qualified Network.HTTP.Client.TLS as HTTP

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




-- integration :: IO ()
-- integration = do
--   WorkerM.runIO $ do
--     creds <- config
--     mgr <- asks manager
--     -- mgr <- debugManager
--     url <- asks (dwollaAuthBaseUrl . env)
--     let clientEnv = mkClientEnv mgr url
--     res <- liftIO $ runClientM (Dwolla.authenticate creds) clientEnv
--     tok <- result res
--     liftIO $ print ("DONE", tok)
--     integrationAuth tok


-- integrationAuth tok = do
--     liftIO $ putStrLn "-- Create Customer ------------"
--     mgr <- asks manager
--     url <- asks (dwollaBaseUrl . env)
--     let clientEnv = mkClientEnv mgr url
--     r <- randomName
--     res <- liftIO $ runClientM (Dwolla.createCustomer tok (customer r)) clientEnv
--     liftIO $ print ("Created", res)
--     custId <- result res
--     dwolla <- dwollaToken
--     liftIO $ print ("DwollaToken", dwolla)

--     fundId <- integrationFundingSource tok dwolla custId

--     transId <- integrationTransfer tok fundId

--     liftIO $ print ("DONE", fundId, transId)

--     pure ()


-- integrationFundingSource tok dwolla custId = do
--     url <- asks (dwollaBaseUrl . env)
--     mgr <- debugManager
--     let clientEnv = mkClientEnv mgr url
--     let source = Dwolla.FundingSource dwolla "test funding source"
--     res <- liftIO $ runClientM (Dwolla.createFundingSource tok custId source) clientEnv
--     liftIO $ print ("Funding Source", res)
--     id <- result res
--     pure id


-- integrationTransfer tok fundId = do
--     url <- asks (dwollaBaseUrl . env)
--     mgr <- asks manager
--     let clientEnv = mkClientEnv mgr url

--     let balance = Dwolla.fundingSource url $ Id "114c60d1-9e0a-441b-ae94-a510b6f1c344"
--     let to = Dwolla.fundingSource url $ fundId
--     let amount = Dwolla.Amount 123.45
--     res <- liftIO $ runClientM (Dwolla.transfer tok balance to amount) clientEnv
--     liftIO $ print ("Transfer", res)
--     id <- result res
--     pure id




-- dwollaToken :: HandlerM (Token Plaid.Dwolla)
-- dwollaToken = do
--     creds <- configs Bank.credentials
--     let access = Bank.Token "access-sandbox-444c2045-c342-46d6-9c18-ab0f17297fd1"
--     let account = Bank.Id "nV4v57dE7RtRwpmQBenvSPXjLMb75mC6XrnAG"
--     res <- Bank.runPlaid $ Plaid.reqDwolla creds access account
--     liftIO $ print ("Plaid", res)
--     pure $ Plaid.processor_token res





-- result res = do
--     case res of
--       Left e  -> throwM e
--       Right v -> pure v


-- debugManager :: HandlerM HTTP.Manager
-- debugManager =
--   liftIO $ HTTP.newManager HTTP.tlsManagerSettings
--     { HTTP.managerModifyRequest = modifyRequest
--     , HTTP.managerModifyResponse = modifyResponse
--     }
--   where
--     modifyRequest req = do
--       print req
--       pure req

--     modifyResponse res = do
--       body <- HTTP.responseBody res
--       putStrLn $ cs body
--       pure res


-- randomName :: MonadIO m => m Text
-- randomName = do
--   s <- liftIO $ randomString (onlyAlphaNum randomASCII) 10
--   pure $ cs s


-- customer rand = Customer
--   { firstName   = "Test"
--   , lastName    = rand
--   , email       = rand <> "@example.com"
--   , ipAddress   = Nothing
--   , type_       = Static
--   , address1    = Address "1234 W Big Street"
--   , address2    = Nothing
--   , city        = "Salt Lake City"
--   , state       = "UT"
--   , postalCode  = "84106"
--   , dateOfBirth = parseDay "1982-08-12"
--   , ssn         = Last4SSN "1234"
--   , phone       = Nothing
--   }
