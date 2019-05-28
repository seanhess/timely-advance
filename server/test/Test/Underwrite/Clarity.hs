{-# LANGUAGE OverloadedStrings #-}
module Test.Underwrite.Clarity where



import Control.Monad.IO.Class    (liftIO)
import Data.String.Conversions   (cs)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Monad
import Text.XML.Parse
import Timely.Underwrite.Clarity (BankBehavior (..), CreditRisk (..), Fraud (..), Inquiry (..), parseBankBehavior, parseCreditRisk, parseInquiry, parseFraud)


specXML = do
  ts <- runTests tests
  defaultMain $ testGroup "tests" $ ts



tests :: Tests ()
tests = do
    group "parsers" $ do
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
        account2InqAppState31_365 bb @?= 18
        account1DefaultRate60Days bb @?= Nothing
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





assertRight :: Show e => Either e a -> IO a
assertRight (Left e)  = assertFailure $ show e
assertRight (Right a) = pure a
