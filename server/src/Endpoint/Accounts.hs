{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
module Endpoint.Accounts where

import Types.Account (Account(..))
import Types.AccountInfo (AccountInfo)
import qualified Types.AccountInfo as AccountInfo
import Types.Id (Id(..), randomId)
import Data.Maybe (listToMaybe)
-- import Data.String.Conversions (cs)
-- import Data.List (find)
import Data.Text (Text)
import Database.Selda




accounts :: Table Account
accounts = table "accounts" [#accountId :- primary]



allAccounts :: (MonadSelda m) => m [Account]
allAccounts = query $ select accounts



newAccount :: (MonadSelda m) => AccountInfo -> m Account
newAccount ai = do
    i <- randomId
    let account = fromAccountInfo i ai
    insert accounts [account]
    pure account


findAccount :: (MonadSelda m) => Id Account -> m (Maybe Account)
findAccount i = do
    as <- query $ do
      account <- select accounts
      restrict (account ! #accountId .== literal i)
      return account
    pure $ listToMaybe as


saveAccount :: (MonadSelda m) => Id Account -> AccountInfo -> m Account
saveAccount i ai = do
    let newAccount = fromAccountInfo i ai
    upsert accounts
      (\account -> account ! #accountId .== literal i)
      (\account -> account `with` updates newAccount)
      [newAccount]
    return newAccount
  where
    updates a =
      [ #firstName := literal (firstName a)
      , #lastName := literal (lastName a)
      , #email := literal (email a)
      ]




fromAccountInfo :: Id Account -> AccountInfo -> Account
fromAccountInfo i ai = Account
    { accountId = i
    , firstName = AccountInfo.firstName ai
    , lastName = AccountInfo.lastName ai
    , email = AccountInfo.email ai
    }



initialize :: MonadSelda m => m ()
initialize = do
    tryDropTable accounts
    tryCreateTable accounts

