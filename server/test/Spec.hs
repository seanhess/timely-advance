import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Monad

import qualified Test.OnboardAccount


main :: IO ()
main =
    defaultMain $ testGroup "tests" $ runTests allTests


allTests :: Tests ()
allTests = do
    group "OnboardAccount" Test.OnboardAccount.tests


