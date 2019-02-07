module Main where

-- import           Control.Concurrent (forkIO, killThread)
import           System.IO (hSetBuffering, stdout, stderr, BufferMode(..))
import           System.Environment (getArgs)
-- import           System.Exit (ExitCode(..))
-- import           System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))
-- import           System.Posix.Process (exitImmediately)

import qualified Timely.Worker.AccountOnboard as AccountOnboard
import qualified Timely.Worker.AccountUpdate as AccountUpdate
import qualified Timely.Worker.AdvanceSend as AdvanceSend
import qualified Timely.Worker.AdvanceCollect as AdvanceCollect
import qualified Timely.Worker.WorkerM as Worker

import qualified Timely.Api as Api

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  a <- getArgs
  case a of
    ["version"        ]          -> putStrLn "TODO version"
    ["api"]                      -> startApi
    ["work-account-onboard"]     -> startAccountOnboard
    ["work-account-update"]      -> startAccountUpdate
    ["work-advance-send"]        -> startAdvanceSend
    ["work-advance-collect"]     -> startAdvanceCollect
    ["schedule-account-update"]  -> startAccountUpdateSchedule
    ["schedule-advance-collect"] -> startAdvanceCollectSchedule
    ["initialize"]               -> Api.initialize
    _                            -> putStrLn "please enter a command"



startApi :: IO ()
startApi = Api.start 3001


startAccountOnboard :: IO ()
startAccountOnboard = Worker.start AccountOnboard.queue AccountOnboard.handler


startAccountUpdate :: IO ()
startAccountUpdate = Worker.start AccountUpdate.queue $ AccountUpdate.handler


startAccountUpdateSchedule :: IO ()
startAccountUpdateSchedule = Worker.runIO AccountUpdate.schedule


startAdvanceSend :: IO ()
startAdvanceSend = Worker.start AdvanceSend.queue AdvanceSend.handler


startAdvanceCollect :: IO ()
startAdvanceCollect = Worker.start AdvanceCollect.queue AdvanceCollect.handler


startAdvanceCollectSchedule :: IO ()
startAdvanceCollectSchedule = Worker.runIO AdvanceCollect.schedule


startTest :: IO ()
startTest = Worker.start AdvanceSend.testQueue AdvanceSend.test

-- startAll :: IO ()
-- startAll = do
--     api <- forkIO $ startApi
--     onb <- forkIO $ startAccountOnboard
--     -- evl <- forkIO $ startAccountUpdate

--     putStrLn "Press any key to exit"
--     installHandler keyboardSignal (Catch (exit api onb evl)) Nothing

--     waitAnyKey
--     exit api onb evl
--   where

--     exit api onb evl = do
--       putStrLn "Exiting..."
--       killThread api
--       killThread onb
--       killThread evl
--       -- exitImmediately ExitSuccess

--     waitAnyKey = do
--       c <- getChar
--       if c == '\n'
--          then waitAnyKey
--          else pure ()
