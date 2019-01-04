{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Accounts.Account where

import Accounts.Application (applications)

import Database.Selda
import Data.Maybe (listToMaybe)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Types.Account (Account(..))
import Types.Account.Bank
import Types.Account.Customer
import Types.Id (Id)




-- there's not actually a table currently
-- accounts :: Table Account
-- accounts = table "accounts" [#accountId :- primary]


customers :: Table Customer
customers = table "accounts_customers" [#accountId :- primary]

banks :: Table Bank
banks = table "accounts_banks" [#accountId :- primary]



allAccounts :: (MonadSelda m) => m [Account]
allAccounts = do
    as <- query $ do
      c <- select customers
      b <- select banks
      restrict (c ! #accountId .== b ! #accountId)
      pure (c ! #accountId :*: b :*: c)
    pure $ fmap account as
  where
    account (i :*: b :*: c) = Account i b c


getAccount :: MonadSelda m => Id Account -> m (Maybe Account)
getAccount i = do
    as <- query $ do
      c <- select customers
      b <- select banks
      restrict (c ! #accountId .== literal i)
      restrict (b ! #accountId .== literal i)
      pure (b :*: c)
    pure $ fmap account $ listToMaybe as
  where
    account (b :*: c) = Account i b c


createCustomer :: MonadSelda m => Customer -> m ()
createCustomer c = do
    insert customers [c]
    pure ()


createBank :: MonadSelda m => Bank -> m ()
createBank b = do
    insert banks [b]
    pure ()



initialize :: (MonadSelda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable applications
    tryCreateTable customers
    tryCreateTable banks


