module Test.Underwrite where

-- import           Test.Tasty
-- import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import qualified Test.Underwrite.Clarity
import qualified Test.Underwrite.Experian


tests :: Tests ()
tests = do
    group "clarity"  Test.Underwrite.Clarity.tests
    group "experian" Test.Underwrite.Experian.tests

