{-# LANGUAGE OverloadedStrings #-}
module Timely.Events where


import Data.Function ((&))
import Network.AMQP.Worker (key, word, Key, Routing)
import Timely.AccountStore.Types (Application, Account)
import Timely.Types.Guid (Guid)


applicationsNew :: Key Routing Application
applicationsNew = key "applications" & word "new"


-- transactionsNew :: Key Routing (Guid Account)
-- transactionsNew = key "transactions" & word "new"


accountsUpdate :: Key Routing (Guid Account)
accountsUpdate = key "accounts" & word "update"

