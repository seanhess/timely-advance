{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Underwrite.Clarity where



import           Control.Monad.IO.Class    (liftIO)
import qualified Data.Model.Money          as Money
import           Data.Model.Valid          (Valid (..))
import           Data.String.Conversions   (cs)
import           Network.Clarity           (Address (..), Employer (..), Frequency (..), BankAccountType(..), InquiryTradelineType(..), InquiryPurposeType(..), Account(..), Consumer(..))
import qualified Network.Clarity           as Clarity
import           Test.Dates                (day)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import           Text.XML.Parse
import           Timely.Underwrite.Clarity (BankBehavior (..), CreditRisk (..), Fraud (..), FraudInsight (..), Inquiry (..), parseBankBehavior, parseCreditRisk, parseFraud, parseFraudInsight, parseInquiry, clarity)

specXML = do
  ts <- runTests tests
  defaultMain $ testGroup "tests" $ ts



tests :: Tests ()
tests = do
  group "parsers" testParsers
  group "request" testRequest






testRequest :: Tests ()
testRequest = do
  group "matches input" $ do

    let doc = Clarity.document account consumer

    test "identification" $ do
      idt <- assertRight $ flip runParserDocument doc $ (,,,,)
        <$> (element "group-id" $ content int)
        <*> (element "account-id" $ content int)
        <*> (element "location-id" $ content int)
        <*> (element "username" $ content text)
        <*> (element "password" $ content text)
      idt @?= (101, 201, 8642, "username", "password")

    test "info" $ do
      info <- assertRight $ flip runParserDocument doc $ (,,)
        <$> (element "inquiry-purpose-type" $ content text)
        <*> (element "inquiry-tradeline-type" $ content text)
        <*> (element "control-file-name" $ content text)
      info @?= ("AR", "C7", "Test_TimelyAdvances")

    test "consumer" $ do
      con <- assertRight $ flip runParserDocument doc $ (,,,,,)
        <$> (element "first-name" $ content text)
        <*> (element "last-name" $ content text)
        <*> (element "social-security-number" $ content text)
        <*> (element "date-of-birth" $ content text)
        <*> (element "drivers-license-number" $ content text)
        <*> (element "drivers-license-state" $ content text)
      con @?= ("John", "Fence", "301001240", "1996-03-06", "193444830", "UT")

    test "address" $ do
      add <- assertRight $ flip runParserDocument doc $ (,,,)
        <$> (element "street-address-1" $ content text)
        <*> (element "city" $ content text)
        <*> (element "state" $ content text)
        <*> (element "zip-code" $ content text)
      add @?= ("1234 W Avenue", "Salt Lake City", "UT", "84108")

    test "more consumer" $ do
      info <- assertRight $ flip runParserDocument doc $ (,,,,,)
        <$> (element "email-address" $ content text)
        <*> (element "home-phone" $ content text)
        <*> (element "cell-phone" $ content text)
        <*> (element "bank-routing-number" $ content text)
        <*> (element "bank-account-number" $ content text)
        <*> (element "bank-account-type" $ content text)
      info @?= ("john.fence@gmail.com", "8015551234", "8015551234", "314977188", "1132243112544", "Checking")

    test "payment" $ do
      pay <- assertRight $ flip runParserDocument doc $ (,,,,)
        <$> (element "employer-name" $ content text)
        <*> (element "net-monthly-income" $ content text)
        <*> (element "date-of-next-payday" $ content text)
        <*> (element "pay-frequency" $ content text)
        <*> (element "loan-amount" $ content text)
      pay @?= ("Everest Auto", "4200.00", "2016-12-15", "Semimonthly", "1000.00")







testParsers :: Tests ()
testParsers = do
  inputXml <- liftIO $ readFile "test/Test/Underwrite/Clarity/sample-response.xml" :: Tests String
  doc <- liftIO $ assertRight $ parseLBS def $ cs inputXml
  let cur = fromDocument doc

  test "inquiry" $ do
    inq <- assertRight $ runParser parseInquiry cur
    bankAccountNumberLength       inq @?= 13
    ssnDistinctFirstLastNameCount inq @?= 31
    ssnFirstLastNameCount         inq @?= 1
    ssnLastNameCount              inq @?= 1
    totalHistoricalInquiries      inq @?= 78

  test "credit-risk" $ do
    risk <- assertRight $ runParser parseCreditRisk cur
    denyCodes risk              @?= "111"

  test "twoProducts " $ do
    (inq, risk) <- assertRight $ runParser ((,) <$> parseInquiry <*> parseCreditRisk) cur

    totalHistoricalInquiries inq  @?= 78
    denyCodes risk                @?= "111"


  test "bank-behavior" $ do
    bb <- assertRight $ runParser parseBankBehavior cur
    account1InqAppState30Days bb @?= 0
    account1DefaultRate61_365 bb @?= Nothing
    account1DefaultRateRatio  bb @?= Nothing
    account1DefaultRate60Days bb @?= Nothing
    account2InqAppState31_365 bb @?= 18
    cbbScore                  bb @?= 581
    cbbReasonCode1            bb @?= Just "BB109(*)"
    cbbReasonCode2            bb @?= Just "BB108(*)"
    cbbReasonCode3            bb @?= Just "BB103"
    cbbReasonCode4            bb @?= Just "BB110(*)"


  test "fraud" $ do
    f <- assertRight $ runParser parseFraud cur
    clearFraudInquiry_threesixtyfiveDaysAgo f @?= 11
    clearFraudPointsTotal_ninetyDaysAgo f @?= 13
    clearFraudPointsTotal_sevenDaysAgo f @?= 10
    clearFraudRatio_sevenDaysRatio_ninetyDaysAgo f @?= 0.76
    clearFraudReasonCodes f @?= ["C205","C214", "C213", "C212"]
    clearFraudScore f @?= 618
    crosstabPointsTotal f @?= 91
    nonScorableReasonCode f @?= Just "xx"
    ssn_bankAccount f @?= 3
    ssn_homePhone f @?= 2
    totalNumberOfFraudIndicators f @?= 4

  test "fraud-insight" $ do
    fi <- assertRight $ runParser parseFraudInsight cur

    score                     fi @?= Nothing
    ratio_1_min_90_days       fi @?= Just 0.57
    ratio_10_mins_90_days     fi @?= Just 0.67
    ratio_1_hr_365_days       fi @?= Just 0.61
    ratio_15_day_90_days      fi @?= Just 0.83
    ratio_30_days_365_days    fi @?= Just 0.72
    ratio_90_days_365_days    fi @?= Just 0.83
    xtab_pts_tot              fi @?= Just 95
    xtab_multiple             fi @?= Just 1.69
    xtab_cell_zip             fi @?= Just 1
    xtab_dl_email             fi @?= Just 2
    xtab_dl_zip               fi @?= Just 2
    xtab_email_ssn            fi @?= Just 1
    xtab_email_zip            fi @?= Just 2
    xtab_hmphone_cell         fi @?= Just 1
    xtab_hmphone_email        fi @?= Just 1
    xtab_hmphone_zip          fi @?= Just 1
    xtab_ssn_hmphone          fi @?= Just 3
    xtab_ssn_home_addr        fi @?= Just 3
    xtab_ssn_zip              fi @?= Just 3
    stab_inq_30_days          fi @?= Just 19
    stab_mthlyincome_365_days fi @?= Just 3
    stab_mthlyincome_90_days  fi @?= Just 3
    stab_zip_365_days         fi @?= Just 3





assertRight :: Show e => Either e a -> IO a
assertRight (Left e)  = assertFailure $ show e
assertRight (Right a) = pure a



sendTestRequest :: IO ()
sendTestRequest = do
  c <- clarity account { username = "timelyadvancetestutility", password = "Cbuckethead1!" } consumer
  putStrLn "CLARITY"
  print c



add = Address "1234 W Avenue" Nothing "Salt Lake City" (Valid "UT") (Valid "84108")
emp = Employer "Everest Auto" Nothing Nothing Nothing
account = Account
  { groupId  = 101
  , accountId = 201
  , locationId = 8642
  , username             = "username"
  , password             = "password"
  , controlFileName      = "Test_TimelyAdvances"
  , inquiryPurposeType   = AR
  , inquiryTradelineType = InquiryTradelineType "C7"
  }
consumer = Consumer
  { firstName            = "John"
  , lastName             = "Fence"
  , middleInitial        = Nothing
  , generationCode       = Nothing
  , socialSecurityNumber = Valid "301001240"
  , dateOfBirth          = day "1996-03-06"
  , driversLicenseNumber = Just "193444830"
  , driversLicenseState  = Just (Valid "UT")
  , bankRoutingNumber    = Valid "314977188"
  , bankAccountNumber    = Valid "1132243112544"
  , bankAccountType      = Checking
  , address              = add
  , emailAddress         = "john.fence@gmail.com"
  , homePhone            = Valid "8015551234"
  , cellPhone            = Valid "8015551234"
  , employer             = Just emp
  , netMonthlyIncome     = Money.fromFloat 4200
  , dateOfNextPayday     = day "2016-12-15"
  , payFrequency         = Semimonthly
  , loanAmount           = Money.fromFloat 1000
    }
