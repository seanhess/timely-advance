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
import qualified Version

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

  a <- Environment.getArgs
  case a of
    ["version"        ]          -> printVersion
    ["api"]                      -> Api.start
    ["work-account-onboard"]     -> AccountOnboard.start
    ["work-account-update"]      -> AccountUpdate.start
    ["work-advance-send"]        -> AdvanceSend.start
    ["work-advance-collect"]     -> AdvanceCollect.start
    ["schedule-advance-collect"] -> AdvanceCollect.startSchedule
    ["initialize"]               -> Api.initialize
    _                            -> putStrLn "please enter a command"


printVersion :: IO ()
printVersion =
  putStrLn $ Version.version








startAll :: IO ()
startAll = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  Async.mapConcurrently_ id
    [ Api.start
    , AccountOnboard.start
    , AccountUpdate.start
    , AdvanceSend.start
    , AdvanceCollect.start
    , AdvanceCollect.startSchedule
    ]
