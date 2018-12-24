{-# LANGUAGE OverloadedStrings #-}
module Events where


import Data.Function ((&))
import Network.AMQP.Worker (key, word, Key, Routing, Exchange)
import qualified Network.AMQP.Worker as Worker
import Types.Account (Account)


appExchange :: Exchange
appExchange = Worker.exchange "app"


accountsNew :: Key Routing Account
accountsNew = key "accounts" & word "new"

