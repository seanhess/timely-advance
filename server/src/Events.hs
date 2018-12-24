{-# LANGUAGE OverloadedStrings #-}
module Events where


import Data.Function ((&))
import Network.AMQP.Worker (key, word, Key, Routing, Exchange)
import qualified Network.AMQP.Worker as Worker
import Types.Account (Account)
import Types.Application (Application)


appExchange :: Exchange
appExchange = Worker.exchange "app"


applicationsNew :: Key Routing Application
applicationsNew = key "applications" & word "new"
