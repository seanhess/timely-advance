module Main where

import           Control.Concurrent (forkIO, killThread)
import           System.IO (hSetBuffering, stdout, stderr, BufferMode(..))
import           System.Environment (getArgs)
-- import           System.Exit (ExitCode(..))
import           System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))
-- import           System.Posix.Process (exitImmediately)

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
    ["api"]             -> startApi
    ["onboard-account"] -> startOnboardAccount
    ["initialize"]      -> Api.initialize
    _                   -> startAll



startApi :: IO ()
startApi = Api.start 3001


startOnboardAccount :: IO ()
startOnboardAccount = Worker.start OnboardAccount.queue OnboardAccount.handler


startAll :: IO ()
startAll = do
    api <- forkIO $ startApi
    onb <- forkIO $ startOnboardAccount

    putStrLn "Press any key to exit"
    installHandler keyboardSignal (Catch (exit api onb)) Nothing

    waitAnyKey
    exit api onb
  where

    exit api onb = do
      putStrLn "Exiting..."
      killThread api
      killThread onb
      -- exitImmediately ExitSuccess

    waitAnyKey = do
      c <- getChar
      if c == '\n'
         then waitAnyKey
         else pure ()
