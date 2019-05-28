module Test.Underwrite where

-- import           Test.Tasty
-- import           Test.Tasty.HUnit
import           Test.Tasty.Monad
import qualified Test.Underwrite.Clarity


tests :: Tests ()
tests = do
    group "clarity" Test.Underwrite.Clarity.tests

