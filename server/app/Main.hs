module Main where

import System.IO (hSetBuffering, stdout, stderr, BufferMode(..))
import System.Environment (getArgs)
import qualified Worker.OnboardAccount as OnboardAccount

import qualified Api
import SeldaTutorial


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    a <- getArgs
    case a of
      ["version"] -> putStrLn $ "TODO version"
      ["onboard-account"] -> OnboardAccount.start
      _ -> Api.start 3001


