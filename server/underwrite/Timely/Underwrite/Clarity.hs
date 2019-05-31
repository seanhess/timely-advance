{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Timely.Underwrite.Clarity where

import           Control.Lens            ((^?))
import qualified Control.Lens            as Lens
import           Control.Monad.Catch     (Exception, MonadCatch, MonadThrow, throwM)
import           Control.Monad.IO.Class  (MonadIO)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Model.Money        (Money, fromFloat)
import           Data.Model.Types        (State)
import           Data.Model.Valid        (Valid, Validate (..))
import           Data.Proxy              (Proxy (..))
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Network.Clarity         as Clarity (Config (..), Consumer (..), document, inquiry, renderXML)
import           Text.Read               (readMaybe)
import           Text.XML.Parse          (Parser, content, element, find, float, int, optional, parseError, runParserDocument, text, ParseError)



-- Request ------------------------------------------------


-- TODO map from our consumer type to theirs
-- TODO send in credentials
-- TODO warehouse the request and response somewhere?
-- TODO catch errors and encrypt PII

data Error = Error ByteString ByteString ParseError
  deriving (Show)
instance Exception Error

clarity :: (MonadIO m, MonadThrow m, MonadCatch m) => Config -> Consumer -> m Clarity
clarity a c = do
  let req = Clarity.document a c
  res <- Clarity.inquiry req

  case runParserDocument parse res of
    Left err -> throwM $ Error (renderXML req) (renderXML res) err
    Right c  -> pure c



data Clarity = Clarity
  { bankBehavior :: BankBehavior
  , creditRisk   :: CreditRisk
  , fraud        :: Fraud
  , fraudInsight :: FraudInsight
  , inquiry      :: Inquiry
  } deriving (Show)


parse :: Parser Clarity
parse = do
  Clarity <$> parseBankBehavior <*> parseCreditRisk <*> parseFraud <*> parseFraudInsight <*> parseInquiry




-- TODO all fields are maybe?
-- TODO move the main parsers into here instead of in Network.Clarity. It just returns a Document


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
  } deriving (Show)


parseBankBehavior :: Parser BankBehavior
parseBankBehavior = element "clear-bank-behavior" $ do

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
  } deriving (Show)


parseFraud :: Parser Fraud
parseFraud = element "clear-fraud" $ do
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




-- Credit Risk ---------------------------------------------

data CreditRisk = CreditRisk
  { denyCodes :: Text
  } deriving (Show)


parseCreditRisk :: Parser CreditRisk
parseCreditRisk = element "clear-credit-risk" $ do
  denyCodes <- element "deny-codes" $ content text
  pure $ CreditRisk {denyCodes}


-- Fraud Insight ------------------------------------
-- fi_

data FraudInsight = FraudInsight
  { score                     :: Maybe Int
  , ratio_1_min_90_days       :: Maybe Float
  , ratio_10_mins_90_days     :: Maybe Float
  , ratio_1_hr_365_days       :: Maybe Float
  , ratio_15_day_90_days      :: Maybe Float
  , ratio_30_days_365_days    :: Maybe Float
  , ratio_90_days_365_days    :: Maybe Float
  , xtab_pts_tot              :: Maybe Int
  , xtab_multiple             :: Maybe Float
  , xtab_cell_zip             :: Maybe Int
  , xtab_dl_email             :: Maybe Int
  , xtab_dl_zip               :: Maybe Int
  , xtab_email_ssn            :: Maybe Int
  , xtab_email_zip            :: Maybe Int
  , xtab_hmphone_cell         :: Maybe Int
  , xtab_hmphone_email        :: Maybe Int
  , xtab_hmphone_zip          :: Maybe Int
  , xtab_ssn_hmphone          :: Maybe Int
  , xtab_ssn_home_addr        :: Maybe Int
  , xtab_ssn_zip              :: Maybe Int
  , stab_inq_30_days          :: Maybe Int
  , stab_mthlyincome_365_days :: Maybe Int
  , stab_mthlyincome_90_days  :: Maybe Int
  , stab_zip_365_days         :: Maybe Int
  } deriving (Show)


parseFraudInsight :: Parser FraudInsight
parseFraudInsight = element "clear-fraud-insight" $ do
  score <- element "score" $ content $ optional int -- Nothing

  ratio_1_min_90_days <- find "ratio" (isName "one_minute_ago") $ element "ninety-days-ago" $ content $ optional float
  ratio_10_mins_90_days <- find "ratio" (isName "ten_minutes_ago") $ element "ninety-days-ago" $ content $ optional float
  ratio_1_hr_365_days <- find "ratio" (isName "one_hour_ago") $ element "threesixtyfive-days-ago" $ content $ optional float
  ratio_15_day_90_days <- find "ratio" (isName "fifteen_days_ago") $ element "ninety-days-ago" $ content $ optional float
  ratio_30_days_365_days <- find "ratio" (isName "thirty_days_ago") $ element "threesixtyfive-days-ago" $ content $ optional float
  ratio_90_days_365_days <- find "ratio" (isName "ninety_days_ago") $ element "threesixtyfive-days-ago" $ content $ optional float

  xtab_pts_tot <- element "crosstab-points-total" $ content $ optional int
  xtab_multiple <- element "crosstab-multiple" $ content $ optional float

  xtab_cell_zip <- find "crosstab" (isName "cell_phone") $ element "zip-code" $ content $ optional int

  (xtab_dl_email, xtab_dl_zip) <- find "crosstab" (isName "drivers_license") $ do
    e <- element "email-address" $ content $ optional int
    z <- element "zip-code" $ content $ optional int
    pure (e, z)

  (xtab_email_ssn, xtab_email_zip) <- find "crosstab" (isName "email_address") $ do
    s <- element "ssn" $ content $ optional int
    z <- element "zip-code" $ content $ optional int
    pure (s, z)

  (xtab_hmphone_cell, xtab_hmphone_email, xtab_hmphone_zip) <- find "crosstab" (isName "home_phone") $ do
    c <- element "cell-phone" $ content $ optional int
    e <- element "email-address" $ content $ optional int
    z <- element "zip-code" $ content $ optional int
    pure (c, e, z)

  (xtab_ssn_hmphone, xtab_ssn_home_addr, xtab_ssn_zip) <- find "crosstab" (isName "ssn") $ do
    p <- element "home-phone" $ content $ optional int
    a <- element "home-address" $ content $ optional int
    z <- element "zip-code" $ content $ optional int
    pure (p, a, z)

  stab_inq_30_days <- find "stability" (isName "inquiry") $ element "thirty-days-ago" $ content $ optional int

  (stab_mthlyincome_90_days, stab_mthlyincome_365_days) <- find "stability" (isName "monthly_income") $ do
    n <- element "ninety-days-ago" $ content $ optional int
    t <- element "threesixtyfive-days-ago" $ content $ optional int
    pure (n, t)

  stab_zip_365_days <- find "stability" (isName "zip_code") $ element "threesixtyfive-days-ago" $ content $ optional int


  pure $ FraudInsight { score , ratio_30_days_365_days , ratio_1_hr_365_days , xtab_pts_tot , ratio_90_days_365_days , ratio_15_day_90_days , xtab_ssn_home_addr , stab_mthlyincome_365_days , xtab_multiple , stab_inq_30_days , xtab_hmphone_email , xtab_ssn_hmphone , xtab_email_zip , xtab_hmphone_cell , ratio_10_mins_90_days , xtab_ssn_zip , xtab_cell_zip , xtab_hmphone_zip , ratio_1_min_90_days , xtab_dl_email , xtab_email_ssn , xtab_dl_zip , stab_zip_365_days , stab_mthlyincome_90_days }








-- Inquiry ---------------------------------------------
-- clarity_inquiry_

data Inquiry = Inquiry
  { bankAccountNumberLength       :: Int
  , ssnDistinctFirstLastNameCount :: Int
  , ssnFirstLastNameCount         :: Int
  , ssnLastNameCount              :: Int
  , totalHistoricalInquiries      :: Int
  } deriving (Show)


parseInquiry :: Parser Inquiry
parseInquiry = element "inquiry" $ do
  bankAccountNumberLength       <- Text.length <$> (element "bank-account-number" $ content text)
  ssnDistinctFirstLastNameCount <- element "ssn-distinct-first-last-name-count" $ content int
  ssnFirstLastNameCount         <- element "ssn-first-last-name-count" $ content int
  ssnLastNameCount              <- element "ssn-last-name-count" $ content int
  totalHistoricalInquiries      <- element "total-historical-inquiries" $ content int
  pure $ Inquiry {bankAccountNumberLength, ssnDistinctFirstLastNameCount, ssnFirstLastNameCount, ssnLastNameCount, totalHistoricalInquiries}







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



isName :: Text -> Parser Bool
isName n1 = do
  n2 <- element "name" $ content text
  pure $ n1 == n2
