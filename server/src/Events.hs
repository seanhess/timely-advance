{-# LANGUAGE OverloadedStrings #-}
module Events where


import Data.Function ((&))
import Network.AMQP.Worker (key, word, Key, Routing)
import AccountStore.Types (Application)


applicationsNew :: Key Routing Application
applicationsNew = key "applications" & word "new"

