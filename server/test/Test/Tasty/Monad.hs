module Test.Tasty.Monad where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Test.Tasty
import Test.Tasty.HUnit

type Tests a = WriterT [TestTree] IO a

runTests :: Tests () -> IO [TestTree]
runTests = execWriterT

group :: String -> Tests () -> Tests ()
group s g = do
  tests <- liftIO $ (runTests g :: IO [TestTree])
  tell [testGroup s tests]

test :: String -> Assertion -> Tests ()
test n a = tell [testCase n a]
