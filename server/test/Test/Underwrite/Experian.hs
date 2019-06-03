{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Underwrite.Experian where


import Data.Aeson                             (Value (String), toJSON, object, (.=))
import Data.Model.Types                       (Address (..))
import Data.Model.Valid                       (Valid (..))
import Network.Experian.CreditProfile.Request (Dob (..))
import Test.Dates                             (day)
-- import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Monad
import Timely.Underwrite                      (Application (..))
import Timely.Underwrite.Experian             (Config (..), Credentials)


tests :: Tests ()
tests = do
  group "applicant" $
    test "dob should serialize" $ do
      toJSON (Dob (day "1937-11-01")) @?= object ["dob" .= String "11011937"]


config :: Credentials -> Config
config = Config "https://uat-us-api.experian.com/" "2912775" "0B"


application :: Application
application = Application
  { phone = Valid "8014139377"
  , firstName = "Timothy"
  , middleName = Just "A"
  , lastName = "Applegate"
  , email = "tim@apples.com"
  , ssn = Valid "666204234"
  , dateOfBirth = day "1937-11-01"
  , address = Address
      { street1 = "2209 KINGSTON DR"
      , street2 = Nothing
      , city = "LAWRENCE"
      , state = Valid "KS"
      , postalCode = Valid "660491614"
      }
  }
