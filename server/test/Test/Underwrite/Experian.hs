{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Underwrite.Experian where


-- import           Test.Tasty
-- import           Test.Tasty.HUnit
import Network.Experian.Address               (Address (Address), State (..), ZipCode (..))
import Network.Experian.CreditProfile.Request (AddOns (..), Applicant (..), Dob (..), Names (..), PermissiblePurpose (..), Request (..), Requestor (..), RiskModels (..), SSN (..), YN (..), ConsumerPii(..))
import Test.Tasty.Monad


tests :: Tests ()
tests = do
  group "TODO" $ test "TODO" $ pure ()
--     group "clarity"  Test.Underwrite.Clarity.tests
--     group "experian" Test.Underwrite.Experian.tests




request :: Request
request = Request {consumerPii, requestor, permissiblePurpose, addOns, customOptions, freezeOverride, resellerInfo}
 where
    consumerPii = ConsumerPii applicant Nothing

    applicant = Applicant
      { name = Names "Applegate" "Timothy" (Just "A") Nothing
      , ssn = Just (SSN "666204234")
      , dob = Just (Dob "11101937")
      , currentAddress = Address "2209 KINGSTON DR" "" "LAWRENCE" (State "KS") (ZipCode "660491614")
      , previousAddress = []
      , driverslicense = Nothing
      , phone = []
      , employment = Nothing
      }

    requestor = Requestor "2912775"

    permissiblePurpose = PermissiblePurpose "0B" Nothing Nothing

    addOns = AddOns
      { directCheck        = YN False
      , riskModels         = RiskModels ["V4"] (YN True)
      , fraudShield        = YN False
      , mla                = YN False
      , ofacmsg            = YN False
      , consumerIdentCheck = Nothing
      , joint              = YN False
      , paymentHistory84   = YN False
      }

    customOptions = Nothing

    freezeOverride = Nothing

    resellerInfo = Nothing
