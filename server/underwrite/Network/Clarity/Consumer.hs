module Network.Clarity.Consumer where

import Data.Model.Types         (Address, Phone, SSN, Valid, State)
import Data.Model.Money         as Money (Money)
import Data.Model.Valid         as Valid (Validate (..), Valid(..), length, digits)
import Data.Text                (Text)
import Data.Time.Calendar       (Day)
import Network.Clarity.Employer (Employer)


data Consumer = Consumer
    { firstName            :: Text
    , lastName             :: Text
    , middleInitial        :: Maybe Text
    , generationCode       :: Maybe GenerationCode
    , socialSecurityNumber :: Valid SSN
    , dateOfBirth          :: Day
    , driversLicenseNumber :: Maybe Text
    , driversLicenseState  :: Maybe (Valid State)
    , bankRoutingNumber    :: Valid RoutingNumber
    , bankAccountNumber    :: Valid AccountNumber
    , bankAccountType      :: BankAccountType
    , address              :: Address
    , emailAddress         :: Text
    , homePhone            :: Valid Phone
    , cellPhone            :: Valid Phone
    , employer             :: Maybe Employer
    , netMonthlyIncome     :: Money
    , dateOfNextPayday     :: Day
    , payFrequency         :: Frequency
    , loanAmount           :: Money
    }



data Frequency
  = Daily
  | Weekly
  | Biweekly
  | Semimonthly
  | Monthly
  deriving (Show)

data BankAccountType
  = Checking
  | Savings
  | DebitCard
  | MoneyMarket
  | Other

data RoutingNumber
data AccountNumber

instance Validate RoutingNumber where
  validate t = do
    Valid.length 9 t
    Valid.digits t
    pure $ Valid t

instance Validate AccountNumber where
  validate t = do
    Valid.digits t
    pure $ Valid t


data GenerationCode
  = Jr
  | Sr
  | G2 -- 2
  | G3
  | G4
  | G5
  | G6
  | G7
  | G8
  | G9
