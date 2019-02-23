{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Concurrent.Async     as Async
import qualified System.Environment           as Environment
import qualified System.IO                    as IO
import qualified Timely.Api                   as Api
import qualified Timely.Worker.AccountOnboard as AccountOnboard
import qualified Timely.Worker.AccountUpdate  as AccountUpdate
import qualified Timely.Worker.AdvanceCollect as AdvanceCollect
import qualified Timely.Worker.AdvanceSend    as AdvanceSend
import qualified Timely.Worker.Schedule       as Schedule
import qualified Timely.Worker.WorkerM        as Worker

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

  a <- Environment.getArgs
  case a of
    ["version"        ]      -> printVersion
    ["api"]                  -> startApi
    ["work-account-onboard"] -> startAccountOnboard
    ["work-account-update"]  -> startAccountUpdate
    ["work-advance-send"]    -> startAdvanceSend
    ["work-advance-collect"] -> startAdvanceCollect
    ["schedule"]             -> startScheduler
    ["initialize"]           -> Api.initialize
    _                        -> putStrLn "please enter a command"


printVersion :: IO ()
printVersion =
  putStrLn $ Api.version



startApi :: IO ()
startApi = Api.start


startAccountOnboard :: IO ()
startAccountOnboard = Worker.start AccountOnboard.queue AccountOnboard.handler


startAccountUpdate :: IO ()
startAccountUpdate = Worker.start AccountUpdate.queue $ AccountUpdate.handler


startAdvanceSend :: IO ()
startAdvanceSend = Worker.start AdvanceSend.queue AdvanceSend.handler


startAdvanceCollect :: IO ()
startAdvanceCollect = Worker.start AdvanceCollect.queue AdvanceCollect.handler


startScheduler :: IO ()
startScheduler = Schedule.start



-- startTest :: IO ()
-- startTest = Worker.start AdvanceSend.testQueue AdvanceSend.test


startAll :: IO ()
startAll = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  Async.mapConcurrently_ id
    [ startApi
    , startAccountOnboard
    , startAccountUpdate
    , startAdvanceSend
    , startAdvanceCollect
    , startScheduler
    ]
