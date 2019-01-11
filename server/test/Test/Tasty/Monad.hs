module Test.Tasty.Monad where

import Control.Monad.Writer (Writer, execWriter, tell)
import Test.Tasty
import Test.Tasty.HUnit

type Tests a = Writer [TestTree] a

runTests :: Tests () -> [TestTree]
runTests = execWriter

group :: String -> Tests () -> Tests ()
group s w = tell [testGroup s $ runTests w]

test :: String -> Assertion -> Tests ()
test n a = tell [testCase n a]
