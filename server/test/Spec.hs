import Test.Tasty
-- import Test.Tasty.HUnit
import Test.Tasty.Monad

import qualified Test.OnboardAccount
import qualified Test.Evaluate
import qualified Test.Advances
import qualified Test.Dwolla
import qualified Test.Notify
import qualified Test.Bank



main :: IO ()
main =
    defaultMain $ testGroup "tests" $ runTests allTests


allTests :: Tests ()
allTests = do
    group "OnboardAccount" Test.OnboardAccount.tests
    group "Evaluate"       Test.Evaluate.tests
    group "Advances"       Test.Advances.tests
    group "Dwolla"         Test.Dwolla.tests
    group "Notify"         Test.Notify.tests
    group "Bank"           Test.Bank.tests


