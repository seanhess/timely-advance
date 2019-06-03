{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Underwrite.Experian
  ( VantageScore(..)
  , Config(..)
  , Error(..)
  , Credentials(..)
  , accessPool
  , vantageScore
  , toRequest
  ) where


import Control.Exception                       (Exception, throwIO)
import Control.Monad.IO.Class                  (MonadIO, liftIO)
import Data.Aeson                              as Aeson (Result (..), Value, encode, fromJSON)
import Data.ByteString.Lazy                    (ByteString)
import Data.List                               as List (find)
import Data.Maybe                              (listToMaybe)
import Data.Model.Types                        as Model (valid, Address(..))
import Data.Pool                               as Pool (Pool, createPool, withResource)
import Data.Text                               (Text)
import Network.Experian.Address                as Exp (Address (Address), State (..), ZipCode (..))
import Network.Experian.CreditProfile          as CreditProfile (AccessToken, Credentials(..), authenticate, load)
import Network.Experian.CreditProfile.Request  as Request (AddOns (AddOns), Applicant (..), ConsumerPii (..), Dob (..), Names (..), PermissiblePurpose (..), Phone (..), PhoneType (..), Request (..), Requestor (Requestor), RiskModels (RiskModels), SSN (..), YN (..))
import Network.Experian.CreditProfile.Response (CreditProfile (..), Response (..), RiskModel (..))
import Timely.Underwrite.Types                 (Application (..))


-- TODO warehouse full data set

newtype VantageScore = VantageScore Text
  deriving (Show)


data Config = Config
  { endpoint       :: String
  , subscriberCode :: Text
  , purposeType    :: Text
  , credentials    :: Credentials
  } deriving (Show)



data Error
  = JSONError String ByteString
  | NoScore ByteString
  deriving (Show)
instance Exception Error



-- TODO, test what happens if one expires!

-- | A pool of access tokens, automatically created on demand
accessPool :: Config -> IO (Pool AccessToken)
accessPool Config {endpoint, credentials} =
    Pool.createPool create destroy numStripes time numTokens
  where
    create = authenticate endpoint credentials
    destroy _ = pure ()
    numStripes = 1
    time = 30*60 -- idle time: before we throw them away
    numTokens = 2




vantageScore :: (MonadIO m) => Config -> Pool AccessToken -> Application -> m VantageScore
vantageScore c@Config {endpoint} tokens app =
  liftIO $ Pool.withResource tokens $ \tok -> do
    val <- CreditProfile.load endpoint tok (toRequest c app)
    parseVantageScore val



toRequest :: Config -> Application -> Request
toRequest Config { subscriberCode, purposeType } Application { phone, firstName, middleName, lastName, ssn, dateOfBirth, address } =
  Request { consumerPii , requestor , permissiblePurpose , addOns , customOptions = Nothing, freezeOverride = Nothing, resellerInfo = Nothing }
  where

    consumerPii = ConsumerPii { primaryApplicant = applicant address, secondaryApplicant = Nothing }

    applicant Model.Address {street1, street2, city, state, postalCode } = Applicant
      { name = Names { firstName, lastName, middleName, generationCode = Nothing }
      , dob = Just (Dob dateOfBirth)
      , ssn = Just (SSN $ valid ssn)
      , currentAddress = Exp.Address street1 street2 city (State $ valid state) (ZipCode $ valid postalCode)
      , previousAddress = []
      , driverslicense = Nothing
      , phone = [ Phone (valid phone) C ]
      , employment = Nothing
      }

    requestor = Requestor subscriberCode

    permissiblePurpose = PermissiblePurpose purposeType Nothing Nothing

    no = YN False
    yes = YN True

    addOns = AddOns (RiskModels ["V4"] yes) no no no no no no Nothing




-- it should throw an error if the vantage score isn't obtainable?
parseVantageScore :: MonadIO m => Value -> m VantageScore
parseVantageScore val = do
  case fromJSON val of
    Error err -> liftIO $ throwIO $ JSONError err (encode val)
    Success res ->
      case findVantageScore res of
        Nothing -> liftIO $ throwIO $ NoScore (encode val)
        Just s  -> pure s


findVantageScore :: Response -> Maybe VantageScore
findVantageScore res = do
    p <- listToMaybe $ creditProfile res
    m <- List.find (\rm -> modelIndicator rm == "V4") $ riskModel p
    pure $ VantageScore $ score m
