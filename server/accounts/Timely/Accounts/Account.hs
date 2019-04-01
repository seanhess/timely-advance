{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Timely.Accounts.Account where


import           Control.Monad.Selda          (Selda, deleteFrom, insert, query)
import           Data.Maybe                   (listToMaybe)
import           Data.Model.Guid
import           Data.Model.Id                (Id)
import           Data.Model.Types             (Phone)
import           Data.Model.Valid             (Valid)
import qualified Data.Time.Clock              as Time
import           Database.Selda               hiding (deleteFrom, insert, query, tryCreateTable)
import           Timely.Accounts.Types
import           Timely.Bank                  (Item)
import           Timely.Evaluate.Types        (Projection (..))






accounts :: Table Account
accounts = table "accounts"
    [ #accountId :- primary
    , #phone     :- index
    , #phone     :- unique
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


allAccounts :: (Selda m) => m [Account]
allAccounts =
    query $ select accounts



getAccount :: Selda m => Guid Account -> m (Maybe Account)
getAccount i = do
    as <- query $ do
      a <- select accounts
      restrict (a ! #accountId .== literal i)
      pure a
    pure $ listToMaybe as


getCustomer :: Selda m => Guid Account -> m (Maybe Customer)
getCustomer i = do
    cs <- query $ do
      c <- select customers
      restrict (c ! #accountId .== literal i)
      pure c
    pure $ listToMaybe cs


getAccountIdByPhone :: Selda m => Valid Phone -> m (Maybe Account)
getAccountIdByPhone p = do
    as <- query $ do
      a <- select accounts
      restrict (a ! #phone .== literal p)
      pure a
    pure $ listToMaybe as


getAccountByBankId :: Selda m => Id Item -> m [Account]
getAccountByBankId i = do
    query $ do
      a <- select accounts
      restrict (a ! #bankItemId .== literal i)
      pure a




getBankAccounts :: Selda m => Guid Account -> m [BankAccount]
getBankAccounts i =
    query $ do
      b <- select banks
      restrict (b ! #accountId .== literal i)
      pure b



setAccountHealth :: Selda m => Guid Account -> Projection -> m ()
setAccountHealth i Projection {expenses, available} = do
    now <- liftIO $ Time.getCurrentTime
    deleteFrom healths (\h -> h ! #accountId .== literal i)
    insert healths [Health {accountId = i, expenses, available, created = now }]
    pure ()


getAccountHealth :: Selda m => Guid Account -> m (Maybe Health)
getAccountHealth i = do
  hs <- query $ do
    b <- select healths
    restrict (b ! #accountId .== literal i)
    pure b
  pure $ listToMaybe hs


createAccount :: Selda m => Account -> Customer -> m ()
createAccount acc cust = do
    -- accounts must be first!
    insert accounts [acc]
    insert customers [cust]
    pure ()




setBankAccounts :: Selda m => Guid Account -> [BankAccount] -> m ()
setBankAccounts i bs = do
    deleteFrom banks (\b -> b ! #accountId .== literal i)
    insert banks bs
    pure ()


