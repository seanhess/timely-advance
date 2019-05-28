module Network.Experian.Request where


-- The sandbox is faked. It doesn't read the input. So we can't experiment with the format very easily. Even if I send nothing it returns with the same information



import Data.Text                (Text)
import Network.Experian.Address (Address, State)


data Request = Request
  { consumerPii        :: ConsumerPii
  , requestor          :: Requestor
  , permissablePurpose :: PermissablePurpose
  , resellerInfo       :: ResellerInfo
  , freezeOverride     :: FreezeOverride
  , addOns             :: AddOns
  , customOptions      :: CustomOptions
  , thinFileIndicator  :: ThinFileIndicator
  }

data ConsumerPii = ConsumerPii
  { primaryApplicant   :: Applicant
  , secondaryApplicant :: Applicant
  }


-- Applicant ---------------------------------------------

data Applicant = Applicant
  { name            :: Names
  , dob             :: Dob
  , ssn             :: SSN
  , driverslicense  :: DriversLicense
  , phone           :: [ Phone ]
  , employment      :: Employment
  , currentAddress  :: Address
  , previousAddress :: [ Address ]
  }

data Names = Names
  { lastName       :: Text
  , firstName      :: Text
  , middleName     :: Text
  , generationCode :: Text
  }

newtype Dob = Dob Text
    -- "dob": {
    --     "dob": "3021945.0"
    -- },

newtype SSN = SSN Text
    -- "ssn": {
    --     "ssn": "999999990"
    -- },

newtype Phone = Phone Text
    -- "phone": [
    --     {
    --         "number": "818.555.1111",
    --         "type": "R"
    --     }
    -- ],

data DriversLicense = DriversLicense
  { number :: Text
  , state  :: State -- "CA"
  }

data Employment = Employment
  { employerName    :: Text
  , employerAddress :: Address
  }



-- Metadata ----------------------------------------------
data Requestor = Requestor
  { subscriberCode :: Text
  }

-- Note: you can swap these out for Ints and it still works
data PermissablePurpose = PermissablePurpose
  { _type             :: Text -- Int = 18
  , terms             :: Text -- Int = 8
  , abbreviatedAmount :: Text -- Int 100
  }

data ResellerInfo = ResellerInfo
  { endUserName :: Text
  }

data FreezeOverride = FreezeOverride
  { primaryApplFreezeOverrideCode   :: Text -- ""
  , secondaryApplFreezeOverrideCode :: Text -- ""
  }

data AddOns = AddOns
  { directCheck        :: Bool -- YN
  , demographics       :: Text -- Empty
  , riskModels         :: RiskModels
  , summaries          :: Text -- "object"
  , fraudShield        :: Bool -- "Y"
  , mla                :: Bool -- "Y"
  , ofacmsg            :: Bool -- "Y"
  , consumerIdentCheck :: ConsumerIdentCheck -- {}
  , joint              :: Bool -- "N"
  , paymentHistory84   :: Bool -- "Y"
  }


data RiskModels = RiskModels
  { modelIndicator  :: [Text] -- [""]
  , scorePercentile :: Bool -- "Y"
  }


data ConsumerIdentCheck = ConsumerIdentCheck
  { getUniqueConsumerIdentifier       :: Bool -- "Y",
  , verifyPrimaryConsumerIdentifier   :: Bool -- "Y",
  , verifySecondaryConsumerIdentifier :: Bool -- "Y"
  }

data CustomOptions = CustomOptions
    -- "customOptions": {
    --     "optionId": [
    --         ""
    --     ]
    -- },

-- 4
newtype ThinFileIndicator = ThinFileIndicator Int
