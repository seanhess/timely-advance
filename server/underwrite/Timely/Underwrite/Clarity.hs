{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Timely.Underwrite.Clarity where

-- import Network.Clarity
import           Control.Lens            ((^?))
import qualified Control.Lens            as Lens
import           Data.Model.Money        (Money, fromFloat)
import           Data.Model.Types        (State)
import           Data.Model.Valid        (Valid, Validate (..))
import           Data.Proxy              (Proxy (..))
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Network.Clarity         as Clarity
import           Text.Read               (readMaybe)
import           Text.XML.Parse          (Parser, content, element, find, float, int, optional, parseError, text)




-- TODO all fields are maybe?


-- Bank Behavior ----------------------------------
-- clarity_cbb_

data BankBehavior = BankBehavior
  { account1DefaultRate60Days :: Maybe Float
  , account1DefaultRate61_365 :: Maybe Float
  , account1DefaultRateRatio  :: Maybe Float
  , account1InqAppState30Days :: Int
  , account2InqAppState31_365 :: Int
  , cbbReasonCode1            :: Maybe Text
  , cbbReasonCode2            :: Maybe Text
  , cbbReasonCode3            :: Maybe Text
  , cbbReasonCode4            :: Maybe Text
  , cbbScore                  :: Int
  }


parseBankBehavior :: Parser BankBehavior
parseBankBehavior = Clarity.bankBehavior $ do

  (account1DefaultRate60Days, account1DefaultRate61_365, account1DefaultRateRatio, account1InqAppState30Days) <- find "account" (isAccountIndex 1) $ do
    a <- element "default-rate-60-days-ago" $ content $ optional float
    b <- element "default-rate-61-365-days-ago" $ content $ optional float
    c <- element "default-rate-ratio" $ content $ optional float
    d <- element "inquiries-app-state-30-days-ago" $ content int
    pure (a, b, c, d)

  account2InqAppState31_365 <- find "account" (isAccountIndex 2) $ element "inquiries-app-state-31-365-days-ago" $ content int

  cbbScore <- element "cbb-score" $ content int

  codes <- element "cbb-reason-codes" $ content piped

  let cbbReasonCode1 = codes ^? Lens.element 0
  let cbbReasonCode2 = codes ^? Lens.element 1
  let cbbReasonCode3 = codes ^? Lens.element 2
  let cbbReasonCode4 = codes ^? Lens.element 3

  pure BankBehavior {account1DefaultRate60Days, account1DefaultRate61_365, account1DefaultRateRatio, account1InqAppState30Days, account2InqAppState31_365, cbbScore, cbbReasonCode1, cbbReasonCode2, cbbReasonCode3, cbbReasonCode4}

  where
    isAccountIndex n = do
      i <- element "account-index" $ content int
      pure $ i == n




-- Clear Fraud --------------------------------------
-- clarity_fraud_

data Fraud = Fraud
  { clearFraudInquiry_threesixtyfiveDaysAgo      :: Int
  , clearFraudPointsTotal_ninetyDaysAgo          :: Int
  , clearFraudPointsTotal_sevenDaysAgo           :: Int
  , clearFraudRatio_sevenDaysRatio_ninetyDaysAgo :: Float
  , clearFraudReasonCodes                        :: [Text] -- Comma-delimited
  , clearFraudScore                              :: Int
  , crosstabPointsTotal                          :: Int
  , nonScorableReasonCode                        :: Maybe Text
  , ssn_bankAccount                              :: Int
  , ssn_homePhone                                :: Int
  , totalNumberOfFraudIndicators                 :: Int
  }


parseFraud :: Parser Fraud
parseFraud = Clarity.fraud $ do
  i365  <- element "clear-fraud-inquiry"$ element "threesixtyfive-days-ago" $ content int
  p90   <- element "clear-fraud-points-total" $ element "ninety-days-ago" $ content int
  p7    <- element "clear-fraud-points-total" $ element "seven-days-ago" $ content int
  r790  <- find "clear-fraud-ratio" (isName "seven_days_ratio") $ element "ninety-days-ago" $ content float
  rcs   <- element "clear-fraud-reason-codes" $ content piped
  score <- element "clear-fraud-score" $ content int

  xtps  <- element "crosstab-points-total" $ content int
  nsrc  <- element "non-scorable-reason-code" $ content $ optional text

  (sba, shp) <- find "clear-fraud-crosstab" (isName "ssn") $ do
    ba <- element "bank-account" $ content int
    hp <- element "home-phone" $ content int
    pure (ba, hp)

  tnfi <- element "total-number-of-fraud-indicators" $ content int

  pure $ Fraud
   { clearFraudInquiry_threesixtyfiveDaysAgo = i365
   , clearFraudPointsTotal_ninetyDaysAgo = p90
   , clearFraudPointsTotal_sevenDaysAgo = p7
   , clearFraudRatio_sevenDaysRatio_ninetyDaysAgo = r790
   , clearFraudReasonCodes = rcs
   , clearFraudScore = score
   , crosstabPointsTotal = xtps
   , nonScorableReasonCode = nsrc
   , ssn_bankAccount = sba
   , ssn_homePhone = shp
   , totalNumberOfFraudIndicators = tnfi
   }


  where
    isName n1 = do
      n2 <- element "name" $ content text
      pure $ n1 == n2




-- Inquiry --------------------------------------------- clarity_inquiry_

data Inquiry = Inquiry
  { bankAccountNumberLength       :: Int
  , ssnDistinctFirstLastNameCount :: Int
  , ssnFirstLastNameCount         :: Int
  , ssnLastNameCount              :: Int
  , totalHistoricalInquiries      :: Int
  }


parseInquiry :: Parser Inquiry
parseInquiry = Clarity.inquiry $ do
  bankAccountNumberLength       <- Text.length <$> (element "bank-account-number" $ content text)
  ssnDistinctFirstLastNameCount <- element "ssn-distinct-first-last-name-count" $ content int
  ssnFirstLastNameCount         <- element "ssn-first-last-name-count" $ content int
  ssnLastNameCount              <- element "ssn-last-name-count" $ content int
  totalHistoricalInquiries      <- element "total-historical-inquiries" $ content int
  pure $ Inquiry {bankAccountNumberLength, ssnDistinctFirstLastNameCount, ssnFirstLastNameCount, ssnLastNameCount, totalHistoricalInquiries}





-- Credit Risk ---------------------------------------------

data CreditRisk = CreditRisk
  { denyCodes :: Text
  }


parseCreditRisk :: Parser CreditRisk
parseCreditRisk = Clarity.creditRisk $ do
  denyCodes <- element "deny-codes" $ content text
  pure $ CreditRisk {denyCodes}



parseState :: Text -> Parser (Valid State)
parseState = parseValid


parseMoney :: Text -> Parser Money
parseMoney t =
  case readMaybe (cs t) of
    Nothing -> parseError "Expected money" t
    Just f  -> pure $ fromFloat f


parseValid :: forall a. Validate a => Text -> Parser (Valid a)
parseValid t =
  case validate t of
    Nothing -> parseError ("Expected " ++ expects (Proxy :: Proxy a)) t
    Just a  -> pure a



piped :: Text -> Parser [Text]
piped t = pure $ Text.split (=='|') t

