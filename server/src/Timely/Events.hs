{-# LANGUAGE OverloadedStrings #-}
module Timely.Events where


import Data.Function ((&))
import Network.AMQP.Worker (key, word, Key, Routing)
import Timely.AccountStore.Types (Application)


applicationsNew :: Key Routing Application
applicationsNew = key "applications" & word "new"

