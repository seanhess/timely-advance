{-# LANGUAGE DeriveGeneric #-}
module Network.Experian.CreditProfile.Response where

import Data.Aeson               (FromJSON)
import Data.Text                (Text)
import GHC.Generics             (Generic)


data Response = Response
  { creditProfile :: [CreditProfile] }
  deriving (Show, Generic)
instance FromJSON Response


data CreditProfile = CreditProfile
  { riskModel :: [RiskModel]
  } deriving (Show, Generic)
instance FromJSON CreditProfile


data RiskModel = RiskModel
  { modelIndicator :: Text
  , score          :: Text
  , scoreFactors   :: [ ScoreFactor ]
  } deriving (Show, Generic)
instance FromJSON RiskModel


data ScoreFactor = ScoreFactor
  { importance :: Text
  , code       :: Text
  } deriving (Show, Generic)
instance FromJSON ScoreFactor


-- type AddressInformation = Value
-- type ConsumerIdentity = Value
-- type EmploymentInformation = Value
-- type Inquiry = Value
-- type SSN = Value


-- data AddressInformation = AddressInformation
--   { city                        :: Text
--   , dwellingType                :: Text
--   , lastReportingSubscriberCode :: Text
--   , source                      :: Text
--   , state                       :: State
--   , streetName                  :: Text
--   , streetPrefix                :: Text
--   , streetSuffix                :: Text
--   , zipCode                     :: ZipCode
--   , timesReported               :: Maybe Text
--   , countyCode                  :: Maybe Text
--   , censusGeoCode               :: Maybe Text
--   , firstReportedDate           :: Maybe Text
--   } deriving (Show, Generic)


-- data
