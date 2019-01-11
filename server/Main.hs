module Main where

import           System.IO (hSetBuffering, stdout, stderr, BufferMode(..))
import           System.Environment (getArgs)

import qualified Worker.OnboardAccount as OnboardAccount
import qualified Worker.WorkerM as Worker

import qualified Api

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  a <- getArgs
  case a of
    ["version"        ] -> putStrLn "TODO version"
    ["onboard-account"] -> startOnboardAccount
    _                   -> startApi



startApi :: IO ()
startApi = Api.start 3001


startOnboardAccount :: IO ()
startOnboardAccount = Worker.start OnboardAccount.queue OnboardAccount.handler
