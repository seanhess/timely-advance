{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
module Endpoint.Accounts
    ( newApplication
    , getAccount
    , initialize
    ) where

import qualified Accounts.Application as App
import Accounts.Account (getAccount, initialize)
import qualified Events (applicationsNew)
import Network.AMQP.Worker (Queue, Exchange)
import Network.AMQP.Worker.Monad (MonadWorker)
import qualified Network.AMQP.Worker.Monad as Worker
import qualified Network.AMQP.Worker as Worker hiding (publish)
import Types.Application (Application)
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








-- These belong in their own files? Or modularly grouped somehow
newApplication :: (MonadWorker m, MonadSelda m) => AccountInfo -> m Application
newApplication info = do
    -- create an application
    accountId <- randomId
    let app = App.fromAccountInfo accountId info

    -- save it
    App.saveApplication app

    -- publish it
    liftIO $ putStrLn $ "PUBLISHING"
    liftIO $ print $ app
    Worker.publish Events.applicationsNew app

    pure app



-- -- TODO you probably can't save the account directly
-- -- save consumer
-- -- save bank info
-- saveAccount :: (MonadSelda m) => Id Account -> AccountInfo -> m Account
-- saveAccount i ai = do
--     let newAccount = fromAccountInfo i ai
--     upsert accounts
--       (\account -> account ! #accountId .== literal i)
--       (\account -> account `with` updates newAccount)
--       [newAccount]
--     return newAccount
--   where
--     -- you aren't allowed to replace the public key (for now)
--     updates a =
--       [ #firstName := literal (Account.firstName a)
--       , #lastName := literal (Account.lastName a)
--       , #email := literal (Account.email a)
--       , #bankBalance := literal (Account.bankBalance a)
--       ]


-- saveBankBalance :: (MonadSelda m) => Id Account -> Int -> m ()
-- saveBankBalance i b = do
--     update accounts
--       (\account -> account ! #accountId .== literal i)
--       (\account -> account `with` [#bankBalance := literal b])
--     pure ()









