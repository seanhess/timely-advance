{-# LANGUAGE OverloadedStrings #-}
module Timely.Events where


import           Data.Function         ((&))
import           Data.Text             (Text)
import           Network.AMQP.Worker   (Key, Routing, key, word)
import           Timely.Accounts.Types (Account, Application)
import           Timely.Advances       (Advance)


health :: Key Routing Text
health = key "health"


applicationsNew :: Key Routing Application
applicationsNew = key "applications" & word "new"


transactionsUpdate :: Key Routing (Account, Int)
transactionsUpdate = key "transactions" & word "update"


advancesActive :: Key Routing Advance
advancesActive = key "advances" & word "active"


advancesDue :: Key Routing Advance
advancesDue = key "advances" & word "due"


test :: Key Routing Text
test = key "test"
