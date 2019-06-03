{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Network.Experian.CreditProfile.Request where


import Data.Aeson               (Options (..), ToJSON (..), Value (String), defaultOptions, genericToJSON, object, (.=))
import Data.Text                (Text)
import Data.Text                (pack)
import Data.Time.Calendar       (Day)
import Data.Time.Format         (defaultTimeLocale, formatTime)
import GHC.Generics             (Generic)
import Network.Experian.Address (Address, State)


data Request = Request
  { consumerPii        :: ConsumerPii
  , requestor          :: Requestor
  , permissiblePurpose :: PermissiblePurpose
  , addOns             :: AddOns
  , customOptions      :: Maybe CustomOptions
  , freezeOverride     :: Maybe FreezeOverride
  , resellerInfo       :: Maybe ResellerInfo
  } deriving (Show, Generic)
instance ToJSON Request


data ConsumerPii = ConsumerPii
  { primaryApplicant   :: Applicant
  , secondaryApplicant :: Maybe Applicant
  } deriving (Show, Generic)
instance ToJSON ConsumerPii



-- Applicant ---------------------------------------------

data Applicant = Applicant
  { name            :: Names
  , dob             :: Maybe Dob
  , ssn             :: Maybe SSN
  , currentAddress  :: Address
  , previousAddress :: [ Address ]
  , driverslicense  :: Maybe DriversLicense
  , phone           :: [ Phone ]
  , employment      :: Maybe Employment
  } deriving (Show, Generic)
instance ToJSON Applicant


data Names = Names
  { lastName       :: Text
  , firstName      :: Text
  , middleName     :: Maybe Text
  , generationCode :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON Names


newtype Dob = Dob Day
  deriving (Show, Generic)
instance ToJSON Dob where
  toJSON (Dob d) =
    object ["dob" .= (String $ pack $ formatTime defaultTimeLocale "%m%d%Y" d) ]


newtype SSN = SSN { ssn :: Text }
  deriving (Show, Generic)
instance ToJSON SSN

-- 7 or 10 digits
-- Codes: Residential, Business, Instition, Fax, Cellular, Pay Phone = T

data Phone = Phone
  { _number :: Text
  , _type   :: PhoneType
  } deriving (Show, Generic)
instance ToJSON Phone where
  toJSON = genericToJSON dropPrefix


data PhoneType
   = R -- ^ Residential
   | B -- ^ Business
   | I -- ^ Institution
   | F -- ^ Fax
   | C -- ^ Cellular
   | T -- ^ Pay Phone
   deriving (Show, Generic)
instance ToJSON PhoneType


data DriversLicense = DriversLicense
  { number :: Text
  , state  :: State -- "CA"
  } deriving (Show, Generic)
instance ToJSON DriversLicense


data Employment = Employment
  { employerName    :: Text
  , employerAddress :: Address
  } deriving (Show, Generic)
instance ToJSON Employment



-- Metadata ----------------------------------------------
data Requestor = Requestor
  { subscriberCode :: Text
  } deriving (Show, Generic)
instance ToJSON Requestor


data PermissiblePurpose = PermissiblePurpose
  { _type             :: Text -- 0B
  , terms             :: Maybe Text
  , abbreviatedAmount :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON PermissiblePurpose where
  toJSON = genericToJSON dropPrefix


data ResellerInfo = ResellerInfo
  { endUserName :: Text
  } deriving (Show, Generic)
instance ToJSON ResellerInfo


data FreezeOverride = FreezeOverride
  { primaryApplFreezeOverrideCode   :: Text -- ""
  , secondaryApplFreezeOverrideCode :: Text -- ""
  } deriving (Show, Generic)
instance ToJSON FreezeOverride


data AddOns = AddOns
  { riskModels         :: RiskModels
  , directCheck        :: YN
  , fraudShield        :: YN
  , mla                :: YN
  , ofacmsg            :: YN
  , joint              :: YN
  , paymentHistory84   :: YN
  , consumerIdentCheck :: Maybe ConsumerIdentCheck -- {}
  -- , demographics       :: Text -- Empty
  -- , summaries          :: Text -- "object"
  } deriving (Show, Generic)
instance ToJSON AddOns


data RiskModels = RiskModels
  { modelIndicator  :: [Text] -- ["V4"]
  , scorePercentile :: YN -- "Y"
  } deriving (Show, Generic)
instance ToJSON RiskModels


data ConsumerIdentCheck = ConsumerIdentCheck
  { getUniqueConsumerIdentifier       :: YN -- "Y",
  , verifyPrimaryConsumerIdentifier   :: YN -- "Y",
  , verifySecondaryConsumerIdentifier :: YN -- "Y"
  } deriving (Show, Generic)
instance ToJSON ConsumerIdentCheck


data CustomOptions = CustomOptions
  { optionId :: [Text]
  } deriving (Show, Generic)
instance ToJSON CustomOptions



newtype YN = YN Bool
  deriving (Show, Generic)
instance ToJSON YN where
  toJSON (YN True)  = String "Y"
  toJSON (YN False) = String "N"


dropPrefix :: Options
dropPrefix = defaultOptions { fieldLabelModifier = dropWhile (=='_') }
