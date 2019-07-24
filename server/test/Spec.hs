module Spec where
import           Test.Tasty
-- import Test.Tasty.HUnit
import           Test.Tasty.Monad

import qualified Test.Advances
import qualified Test.Bank
import qualified Test.Dwolla
import qualified Test.Evaluate
import qualified Test.Field
import qualified Test.Notify
import qualified Test.OnboardAccount
import qualified Test.UpdateAccount
import qualified Test.Underwrite
import qualified Test.XML



spec :: IO ()
spec = do
    ts <- runTests allTests
    defaultMain $ testGroup "tests" ts


allTests :: Tests ()
allTests = do
    group "OnboardAccount" Test.OnboardAccount.tests
    group "UpdateAccount"  Test.UpdateAccount.tests
    group "Evaluate"       Test.Evaluate.tests
    group "Advances"       Test.Advances.tests
    group "Dwolla"         Test.Dwolla.tests
    group "Notify"         Test.Notify.tests
    group "Bank"           Test.Bank.tests
    group "Field"          Test.Field.tests
    group "XML"            Test.XML.tests
    group "Underwrite"     Test.Underwrite.tests

