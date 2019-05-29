{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Network.Clarity.Request where

-- https://login.clarityservices.com/interactive_xmls/inquiry
-- https://login.clarityservices.com/interactive_xmls/inquiry_response

import           Data.Model.Money        (Money)
import qualified Data.Model.Money        as Money
import           Data.Model.Types        (Address (..), Phone, SSN, State)
import           Data.Model.Valid        (Valid (..), Validate (..))
import qualified Data.Model.Valid        as Valid
import           Data.String.Conversions (cs)
import           Data.Text               (Text, pack)
import           Data.Time.Calendar      (Day)
import           Data.Time.Format        (defaultTimeLocale, formatTime)
import           Prelude                 hiding (length)
import           Text.XML                (Document)
import           Text.XML.Writer         (XML, content, element)
import qualified Text.XML.Writer         as XML


-- TODO check for errors


document :: Request -> Document
document r = XML.document "inquiry" $ do
  element "group-id" $ content $ formatId $ groupId r
  element "account-id" $ content $ formatId $ accountId r
  element "location-id" $ content $ formatId $ locationId r
  element "username" $ content $ username r
  element "password" $ content $ password r
  element "control-file-name" $ content $ controlFileName r
  element "inquiry-purpose-type" $ content $ formatInquiryPurposeType $ inquiryPurposeType r
  element "inquiry-tradeline-type" $ content $ itt $ inquiryTradelineType r
  element "first-name" $ content $ firstName r
  element "last-name" $ content $ lastName r
  element "middle-initial" . content <?> middleInitial r
  element "generation-code" . content . formatGenerationCode <?> generationCode r
  element "email-address" $ content $ emailAddress r
  element "social-security-number" $ content $ valid $ socialSecurityNumber r
  element "date-of-birth" $ content $ formatDate $ dateOfBirth r
  element "drivers-license-number" . content <?> driversLicenseNumber r
  element "drivers-license-state" . content .valid <?> driversLicenseState r
  element "bank-routing-number" $ content $ valid $ bankRoutingNumber r
  element "bank-account-number" $ content $ valid $ bankAccountNumber r
  element "bank-account-type" $ content $ formatBankAccountType $ bankAccountType r
  xmlAddress $ address (r :: Request)
  element "home-phone" $ content $ valid $ homePhone r
  element "cell-phone" $ content $ valid $ cellPhone r
  mapM_ xmlEmployer $ employer r
  element "net-monthly-income" $ content $ Money.formatFloat $ netMonthlyIncome r
  element "date-of-next-payday" $ content $ formatDate $ dateOfNextPayday r
  element "pay-frequency" $ content $ formatFrequency $ payFrequency r
  element "loan-amount" $ content $ Money.formatFloat $ loanAmount r



xmlAddress :: Address -> XML
xmlAddress a = do
  element "street-address-1" $ street1 a
  element "street-address-2" $ street2 a
  element "city" $ city (a :: Address)
  element "state" $ valid $ state (a :: Address)
  element "zip-code" $ valid $ postalCode a


xmlEmployer :: Employer -> XML
xmlEmployer e = do
  element "employer-name" $ name e
  element "employer-address" <?> address (e :: Employer)
  element "employer-city" <?> city (e :: Employer)
  element "employer-state" . valid <?> state (e :: Employer)


formatId :: Int -> Text
formatId = cs . show

formatDate :: Day -> Text
formatDate =
    pack . formatTime defaultTimeLocale "%Y-%m-%d"

-- POST https://secure.clarityservices.com/test_inquiries

data Frequency
  = Daily
  | Weekly
  | Biweekly
  | Semimonthly
  | Monthly
  deriving (Show)


formatFrequency :: Frequency -> Text
formatFrequency = cs . show


-- I need to peek at the documentation to set this up.

data InquiryPurposeType
  = AR -- ^ New Credit
  | AS -- ^ New Credit Soft
  | RA -- ^ Account Review Soft
  | RP -- ^ Consumer Inquiry Soft
  | CL -- ^ Collection Inquiry
  | PC -- ^ Pre-check Soft
  | MS -- ^ Credit Monitor Soft
  | CC -- ^ Check Cash
  | CS -- ^ Collection Soft
  | PS -- ^ Pre-screen Soft
  | IV -- ^ Item Verification
  | IS -- ^ Item Verification Soft
  | EH -- ^ Employment
  | ES -- ^ Employment Soft
  | LH -- ^ Lease
  | LS -- ^ Lease Soft
  | WS -- ^ Written Authorization Soft
  deriving (Show)

formatInquiryPurposeType :: InquiryPurposeType -> Text
formatInquiryPurposeType = cs . show

data BankAccountType
  = Checking
  | Savings
  | DebitCard
  | MoneyMarket
  | Other

formatBankAccountType :: BankAccountType -> Text
formatBankAccountType Checking    = "Checking"
formatBankAccountType Savings     = "Savings"
formatBankAccountType DebitCard   = "Debit Card"
formatBankAccountType MoneyMarket = "Money Market"
formatBankAccountType Other       = "Other"


-- Tons of options, see docs
newtype InquiryTradelineType = InquiryTradelineType { itt :: Text }

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

formatGenerationCode :: GenerationCode -> Text
formatGenerationCode Jr = "Jr"
formatGenerationCode Sr = "Sr"
formatGenerationCode G2 = "G2"
formatGenerationCode G3 = "G3"
formatGenerationCode G4 = "G4"
formatGenerationCode G5 = "G5"
formatGenerationCode G6 = "G6"
formatGenerationCode G7 = "G7"
formatGenerationCode G8 = "G8"
formatGenerationCode G9 = "G9"

data Request = Request
    { groupId              :: Int
    , accountId            :: Int
    , locationId           :: Int
    , username             :: Text
    , password             :: Text
    , controlFileName      :: Text
    , inquiryPurposeType   :: InquiryPurposeType
    , inquiryTradelineType :: InquiryTradelineType
    , firstName            :: Text
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

-- prefix employer
data Employer = Employer
    { name    :: Text
    , address :: Maybe Text
    , city    :: Maybe Text
    , state   :: Maybe (Valid State)
    }


-- , streetAddress1 :: Text
-- , streetAddress2 :: Text
-- , city :: Text
-- , state :: Text -- State
-- , zipCode :: Text




(<?>) :: Monad m => (a -> m ()) -> Maybe a -> m ()
(<?>) = mapM_

infixr 0 <?>
