{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module AccountStore.Account (AccountStore(..), initialize) where

import AccountStore.Types

import Database.Selda
import Data.Maybe (listToMaybe)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Control.Monad.Effect (Effect(..))
import GHC.Generics (Generic)

import Types.Guid




data AccountStore a where
    All            :: AccountStore [Account]
    Find           :: Guid Account -> AccountStore (Maybe Account)
    BankAccounts   :: Guid Account -> AccountStore [BankAccount]

    CreateCustomer :: Customer -> AccountStore ()
    CreateBank     :: BankAccount -> AccountStore ()


instance (MonadSelda m) => Effect m AccountStore where
    run All                = allAccounts
    run (Find i)           = getAccount i
    run (BankAccounts i)   = getBankAccounts i
    run (CreateCustomer c) = createCustomer c
    run (CreateBank b)     = createBank b


customers :: Table Customer
customers = table "accounts_customers" [#id :- autoPrimary]

banks :: Table BankAccount
banks = table "accounts_banks" [#id :- autoPrimary]

accounts :: Table AccountRow
accounts = table "accounts" [#accountId :- primary]



allAccounts :: (MonadSelda m) => m [Account]
allAccounts = do
    as <- query $ do
      c <- select customers
      a <- select accounts
      restrict (a ! #accountId .== c ! #accountId)
      pure (a :*: c)
    pure $ fmap account as
  where
    account (AccountRow {..} :*: customer) = Account {..}


getAccount :: MonadSelda m => Guid Account -> m (Maybe Account)
getAccount i = do
    as <- query $ do
      c <- select customers
      a <- select accounts
      restrict (a ! #accountId .== literal i)
      restrict (c ! #accountId .== literal i)
      pure (a :*: c)
    pure $ account <$> listToMaybe as
  where
    account (AccountRow {..} :*: customer) = Account {..}


getBankAccounts :: MonadSelda m => Guid Account -> m [BankAccount]
getBankAccounts i =
    query $ do
      b <- select banks
      restrict (b ! #accountId .== literal i)
      pure b


createCustomer :: MonadSelda m => Customer -> m ()
createCustomer c = do
    insert customers [c]
    pure ()


createBank :: MonadSelda m => BankAccount -> m ()
createBank b = do
    insert banks [b]
    pure ()



initialize :: (MonadSelda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable accounts
    tryCreateTable customers
    tryCreateTable banks


