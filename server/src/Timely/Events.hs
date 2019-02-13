{-# LANGUAGE OverloadedStrings #-}
module Timely.Events where


import           Data.Function             ((&))
import           Data.Model.Guid           (Guid)
import           Data.Text                 (Text)
import           Network.AMQP.Worker       (Key, Routing, key, word)
import           Timely.AccountStore.Types (Account, Application)
import           Timely.Advances           (Advance)


applicationsNew :: Key Routing Application
applicationsNew = key "applications" & word "new"


transactionsNew :: Key Routing (Guid Account)
transactionsNew = key "transactions" & word "new"


advancesActive :: Key Routing Advance
advancesActive = key "advances" & word "active"


advancesDue :: Key Routing Advance
advancesDue = key "advances" & word "due"


test :: Key Routing Text
test = key "test"
