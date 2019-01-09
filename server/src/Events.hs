{-# LANGUAGE OverloadedStrings #-}
module Events where


import Data.Function ((&))
import Network.AMQP.Worker (key, word, Key, Routing)
import qualified Network.AMQP.Worker as Worker
import Types.Account (Account)
import AccountStore.Types (Application)


applicationsNew :: Key Routing Application
applicationsNew = key "applications" & word "new"

