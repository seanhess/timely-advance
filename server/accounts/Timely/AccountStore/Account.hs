{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Timely.AccountStore.Account (AccountStore(..), initialize, account) where


import           Control.Monad.Selda       (Selda, deleteFrom, insert, query, tryCreateTable)
import           Control.Monad.Service     (Service (..))
import           Data.Maybe                (listToMaybe)
import           Data.Model.Guid
import           Data.Model.Money
import           Database.Selda            hiding (deleteFrom, insert, query, tryCreateTable)

import           Timely.AccountStore.Types
import           Timely.Bank               (Access, Token)
import           Timely.Evaluate.Types     (Projection (..))
import           Timely.Types.Private




data AccountStore a where
    All            :: AccountStore [AccountRow]
    Find           :: Guid Account -> AccountStore (Maybe Account)
    FindByPhone    :: Phone -> AccountStore (Maybe (Guid Account))
    BankAccounts   :: Guid Account -> AccountStore [BankAccount]
    SetHealth      :: Guid Account -> Projection -> AccountStore ()

    CreateAccount  :: Account -> AccountStore ()
    SetBankAccounts :: Guid Account -> [BankAccount] -> AccountStore ()


instance (Selda m) => Service m AccountStore where
    run All                    = allAccounts
    run (Find i)               = getAccount i
    run (FindByPhone p)        = getAccountIdByPhone p
    run (BankAccounts i)       = getBankAccounts i
    run (CreateAccount a)      = createAccount a
    run (SetBankAccounts i bs) = setBankAccounts i bs
    run (SetHealth i p)        = setHealth i p



accounts :: Table AccountRow
accounts = table "accounts"
    [ #accountId :- primary
    , #phone :- index
    , #phone :- unique
    ]

customers :: Table Customer
customers = table "accounts_customers" [#id :- autoPrimary, #accountId :- foreignKey accounts #accountId ]

banks :: Table BankAccount
banks = table "accounts_banks" [#id :- autoPrimary, #accountId :- foreignKey accounts #accountId]

healths :: Table Health
healths = table "accounts_healths"
    [ #accountId :- primary
    , #accountId :- foreignKey accounts #accountId
    ]


allAccounts :: (Selda m) => m [AccountRow]
allAccounts =
    query $ select accounts



getAccount :: Selda m => Guid Account -> m (Maybe Account)
getAccount i = do
    as <- query $ do
      c <- select customers
      a <- select accounts
      h <- select healths
      restrict (a ! #accountId .== literal i)
      restrict (c ! #accountId .== literal i)
      restrict (h ! #accountId .== literal i)
      pure (a :*: c :*: h)
    pure $ account <$> listToMaybe as
  where
    account (AccountRow {..} :*: customer :*: health) = Account {..}


getAccountIdByPhone :: Selda m => Phone -> m (Maybe (Guid Account))
getAccountIdByPhone p = do
    as <- query $ do
      a <- select accounts
      restrict (a ! #phone .== literal p)
      pure $ a ! #accountId
    pure $ listToMaybe as




getBankAccounts :: Selda m => Guid Account -> m [BankAccount]
getBankAccounts i =
    query $ do
      b <- select banks
      restrict (b ! #accountId .== literal i)
      pure b



setHealth :: Selda m => Guid Account -> Projection -> m ()
setHealth i Projection {..} = do
    let accountId = i
    deleteFrom healths (\h -> h ! #accountId .== literal i)
    insert healths [Health {..}]
    pure ()


createAccount :: Selda m => Account -> m ()
createAccount acc = do
    -- accounts must be first!
    insert accounts [accountRow acc]
    insert customers [customer acc]
    insert healths [health acc]
    pure ()


account :: Guid Account -> Phone -> Customer -> Token Access -> Money -> Projection -> Account
account accountId phone customer tok credit Projection {..} = Account {..}
  where bankToken = Private tok
        health = Health {..}


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
    tryCreateTable healths
