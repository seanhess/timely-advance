{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module AccountStore.Account (AccountStore(..), initialize) where

import AccountStore.Types

import Control.Monad.Selda (Selda, query, insert, deleteFrom)
import Database.Selda hiding (query, insert, deleteFrom)
import Data.Maybe (listToMaybe)
import Control.Monad.Effect (Effect(..))

import Types.Guid
import Bank (Token, Access)
import Types.Private




data AccountStore a where
    All            :: AccountStore [Account]
    Find           :: Guid Account -> AccountStore (Maybe Account)
    BankAccounts   :: Guid Account -> AccountStore [BankAccount]

    CreateAccount  :: Application -> Token Access -> AccountStore ()
    SetBankAccounts :: Guid Account -> [BankAccount] -> AccountStore ()


instance (Selda m) => Effect m AccountStore where
    run All                = allAccounts
    run (Find i)           = getAccount i
    run (BankAccounts i)   = getBankAccounts i
    run (CreateAccount a t) = createAccount a t
    run (SetBankAccounts i bs) = setBankAccounts i bs


customers :: Table Customer
customers = table "accounts_customers" [#id :- autoPrimary]

banks :: Table BankAccount
banks = table "accounts_banks" [#id :- autoPrimary]

accounts :: Table AccountRow
accounts = table "accounts" [#accountId :- primary]



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


createAccount :: Selda m => Application -> Token Access -> m ()
createAccount app tok = do
    let acc = newAccount app tok
    insert customers [customer acc]
    insert accounts [accountRow acc]
    pure ()


newAccount :: Application -> Token Access -> Account
newAccount Application {..} tok = Account {..}
  where id = def
        customer = Customer {..}
        bankToken = Private tok


accountRow :: Account -> AccountRow
accountRow Account {..} = AccountRow {..}



setBankAccounts :: Selda m => Guid Account -> [BankAccount] -> m ()
setBankAccounts i bs = do
    deleteFrom banks (\b -> b ! #accountId .== literal i)
    insert banks bs
    pure ()



initialize :: (MonadSelda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable accounts
    tryCreateTable customers
    tryCreateTable banks


