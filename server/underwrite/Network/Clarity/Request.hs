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

import           Control.Lens             ((^.))
import           Control.Monad.Catch      (Exception, MonadCatch, MonadThrow, catch, throwM)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.Model.Money         as Money
import           Data.Model.Types         (Address (..))
import           Data.Model.Valid         (valid)
import           Data.String.Conversions  (cs)
import           Data.Text                (Text, pack)
import           Data.Time.Calendar       (Day)
import           Data.Time.Format         (defaultTimeLocale, formatTime)
import           Network.Clarity.Config  (Config (..), InquiryPurposeType (..))
import           Network.Clarity.Consumer (BankAccountType (..), Consumer (..), Frequency (..), GenerationCode (..))
import           Network.Clarity.Employer (Employer (..))
import           Network.HTTP.Client      as HTTP (HttpException (..), RequestBody (..))
import           Network.Wreq             as Wreq (post, responseBody, Payload(Raw))
import           Prelude                  hiding (length)
import           Text.XML                 as XML (Document, def, parseLBS, renderLBS)
import           Text.XML.Writer          (XML, content, element)
import qualified Text.XML.Writer          as XML




-- TODO check for errors, surface them instead of just a parse error

-- data Endpoint
--   = Test
--   | Live


test :: String
test = "https://secure.clarityservices.com/test_inquiries"



data Error
  = HttpError ByteString HttpException
  | ParseError ByteString ByteString
  deriving (Show)
instance Exception Error


newtype Body = Body Document


inquiry :: (MonadIO m, MonadThrow m, MonadCatch m) => Document -> m Document
inquiry doc = do
  let body = renderXML doc
  let payload = Raw "text/xml" (RequestBodyLBS body)
  res <- (liftIO $ Wreq.post test payload) `catch` (onHttpException body)
  let resBody = res ^. responseBody
  case XML.parseLBS def resBody of
    Left _ -> throwM $ ParseError body resBody
    Right doc -> pure doc


onHttpException :: MonadThrow m => ByteString -> HttpException -> m a
onHttpException body ex = throwM $ HttpError body ex

onParseException :: MonadThrow m => ByteString -> ByteString -> m a
onParseException body resBody = throwM $ ParseError body resBody




renderXML :: Document -> ByteString
renderXML = XML.renderLBS def


-- TODO check for errors

document :: Config -> Consumer -> Document
document a c = XML.document "inquiry" $ do
  xmlConfig a
  xmlConsumer c

xmlConfig :: Config -> XML
xmlConfig r = do
  element "group-id" $ content $ formatId $ groupId r
  element "account-id" $ content $ formatId $ accountId r
  element "location-id" $ content $ formatId $ locationId r
  element "username" $ content $ username r
  element "password" $ content $ password r
  element "control-file-name" $ content $ controlFileName r
  element "inquiry-purpose-type" $ content $ formatInquiryPurposeType $ inquiryPurposeType r
  element "inquiry-tradeline-type" $ content $ inquiryTradelineType r

xmlConsumer :: Consumer -> XML
xmlConsumer r = do
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
  xmlAddress $ address (r :: Consumer)
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



formatFrequency :: Frequency -> Text
formatFrequency = cs . show



formatInquiryPurposeType :: InquiryPurposeType -> Text
formatInquiryPurposeType = cs . show


formatBankAccountType :: BankAccountType -> Text
formatBankAccountType Checking    = "Checking"
formatBankAccountType Savings     = "Savings"
formatBankAccountType DebitCard   = "Debit Card"
formatBankAccountType MoneyMarket = "Money Market"
formatBankAccountType Other       = "Other"


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



-- , streetAddress1 :: Text
-- , streetAddress2 :: Text
-- , city :: Text
-- , state :: Text -- State
-- , zipCode :: Text




(<?>) :: Monad m => (a -> m ()) -> Maybe a -> m ()
(<?>) = mapM_

infixr 0 <?>

