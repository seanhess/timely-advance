{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module AccountStore.Account (AccountStore(..), initialize, account) where

import AccountStore.Types

import Control.Monad.Selda (Selda, query, insert, deleteFrom, tryCreateTable)
import Database.Selda hiding (query, insert, deleteFrom, tryCreateTable)
import Data.Maybe (listToMaybe)
import Control.Monad.Service (Service(..))

import Types.Guid
import Bank (Token, Access)
import Types.Private




data AccountStore a where
    All            :: AccountStore [Account]
    Find           :: Guid Account -> AccountStore (Maybe Account)
    BankAccounts   :: Guid Account -> AccountStore [BankAccount]

    CreateAccount  :: Account -> AccountStore ()
    SetBankAccounts :: Guid Account -> [BankAccount] -> AccountStore ()


instance (Selda m) => Service m AccountStore where
    run All                = allAccounts
    run (Find i)           = getAccount i
    run (BankAccounts i)   = getBankAccounts i
    run (CreateAccount a) = createAccount a
    run (SetBankAccounts i bs) = setBankAccounts i bs



accounts :: Table AccountRow
accounts = table "accounts" [#accountId :- primary]

customers :: Table Customer
customers = table "accounts_customers" [#id :- autoPrimary, #accountId :- foreignKey accounts #accountId ]

banks :: Table BankAccount
banks = table "accounts_banks" [#id :- autoPrimary, #accountId :- foreignKey accounts #accountId]


allAccounts :: (Selda m) => m [Account]
allAccounts = do
    as <- query $ do
      c <- select customers
      a <- select accounts
      restrict (a ! #accountId .== c ! #accountId)
      pure (a :*: c)
    pure $ fmap account as
  where
    account (AccountRow {..} :*: customer) = Account {..}


getAccount :: Selda m => Guid Account -> m (Maybe Account)
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


getBankAccounts :: Selda m => Guid Account -> m [BankAccount]
getBankAccounts i =
    query $ do
      b <- select banks
      restrict (b ! #accountId .== literal i)
      pure b


createAccount :: Selda m => Account -> m ()
createAccount acc = do
    -- accounts must be first!
    insert accounts [accountRow acc]
    insert customers [customer acc]
    pure ()


account :: Guid Account -> Customer -> Token Access -> Account
account accountId customer tok = Account {..}
  where bankToken = Private tok


accountRow :: Account -> AccountRow
accountRow Account {..} = AccountRow {..}



setBankAccounts :: Selda m => Guid Account -> [BankAccount] -> m ()
setBankAccounts i bs = do
    deleteFrom banks (\b -> b ! #accountId .== literal i)
    insert banks bs
    pure ()



initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable accounts
    tryCreateTable customers
    tryCreateTable banks


