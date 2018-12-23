{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
module Endpoint.Accounts where

import Network.AMQP.Worker (Queue, Direct, Exchange)
import Network.AMQP.Worker.Monad (MonadWorker)
import qualified Network.AMQP.Worker.Monad as MWorker
import qualified Network.AMQP.Worker as Worker
import Types.Account (Account(..))
import qualified Types.Account as Account
import Types.AccountInfo (AccountInfo(..))
import qualified Types.AccountInfo as AccountInfo
import Types.Id (Id(..), randomId)
import Data.Maybe (listToMaybe)
-- import Data.String.Conversions (cs)
-- import Data.List (find)
import Data.Text (Text)
import Database.Selda



-- AMQP Stuff
exchange :: Exchange
exchange = Worker.exchange "app"

queueAccountsCreated :: Queue Direct Account
queueAccountsCreated = Worker.queue exchange "accounts.created"



accounts :: Table Account
accounts = table "accounts" [#accountId :- primary]



allAccounts :: (MonadSelda m) => m [Account]
allAccounts = query $ select accounts



newAccount :: (MonadWorker m, MonadSelda m) => AccountInfo -> m Account
newAccount ai = do
    i <- randomId
    let account = fromAccountInfo i ai
    insert accounts [account]

    -- publish our message!
    -- TODO where do these belong? Not here, it's the consumer who is responsible to initialize queues. This is obviously BS. It should be automatic
    MWorker.initQueue queueAccountsCreated

    MWorker.publish queueAccountsCreated account

    -- TODO send a message to "accounts.created"
    -- TODO create a worker that picks that up
    -- TODO write a driver that can call plaid to get the bank info
    -- TODO update the bank balance here
    pure account


findAccount :: (MonadSelda m) => Id Account -> m (Maybe Account)
findAccount i = do
    as <- query $ do
      account <- select accounts
      restrict (account ! #accountId .== literal i)
      return account
    pure $ listToMaybe as


-- TODO you probably can't save the account directly
-- save consumer
-- save bank info
saveAccount :: (MonadSelda m) => Id Account -> AccountInfo -> m Account
saveAccount i ai = do
    let newAccount = fromAccountInfo i ai
    upsert accounts
      (\account -> account ! #accountId .== literal i)
      (\account -> account `with` updates newAccount)
      [newAccount]
    return newAccount
  where
    -- you aren't allowed to replace the public key (for now)
    updates a =
      [ #firstName := literal (Account.firstName a)
      , #lastName := literal (Account.lastName a)
      , #email := literal (Account.email a)
      ]




fromAccountInfo :: Id Account -> AccountInfo -> Account
fromAccountInfo i AccountInfo {..} = Account {..}
  where
    accountId = i
    bankBalance = 0




initialize :: (MonadSelda m, MonadIO m) => m ()
initialize =
    -- drop the table / db first to run migrations
    tryCreateTable accounts



