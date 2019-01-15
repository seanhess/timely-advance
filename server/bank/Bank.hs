{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Bank
    ( Token(..)
    , Id(..)
    , Access
    , Public
    , Account(..)
    , Currency(..)
    , CurrencyCode(..)
    , Balances(..)
    , AccountType(..)
    , AccountSubType(..)
    , Identity(..)
    , Address(..)
    , IdentityInfo(_data, _primary)
    , AddressInfo(..)
    , Banks(..)
    ) where

import Control.Monad.Service (Service(..))
import Control.Monad.Plaid (MonadPlaid, runPlaid)
import qualified Control.Monad.Plaid as Plaid
import Network.Plaid.Types
import qualified Network.Plaid as Plaid
import Data.Time.Calendar (fromGregorian)

-- Bank Service

data Banks a where
    Authenticate     :: Token Public -> Banks (Token Access)
    LoadIdentity     :: Token Access -> Banks Identity
    LoadAccounts     :: Token Access -> Banks [Account]
    LoadTransactions :: Token Access -> Id Account -> Banks [Transaction]


-- there's an obvious implementation for anyone who has a MonadSelda
instance (MonadPlaid m) => Service m Banks where
    run (Authenticate t)      = authenticate t
    run (LoadIdentity t)      = loadIdentity t
    run (LoadAccounts t)      = loadAccounts t
    run (LoadTransactions t i) = loadTransactions t i


authenticate :: MonadPlaid m => Token Public -> m (Token Access)
authenticate pub = do
    creds <- Plaid.credentials
    res <- runPlaid $ Plaid.reqExchangeToken creds pub
    pure $ access_token (res :: ExchangeTokenResponse)


loadIdentity :: MonadPlaid m => Token Access -> m Identity
loadIdentity tok = do
    creds <- Plaid.credentials
    res <- runPlaid $ Plaid.reqIdentity creds tok
    pure $ identity (res :: IdentityResponse)


loadAccounts :: MonadPlaid m => Token Access -> m [Account]
loadAccounts tok = do
    creds <- Plaid.credentials
    res <- runPlaid $ Plaid.reqAccounts creds tok
    pure $ accounts (res :: AccountsResponse)


-- which transactions should I load? How far back? 3 months?
-- this only works for specific accounts
-- so we don't really care about the account information, do we?
loadTransactions :: MonadPlaid m => Token Access -> Id Account -> m [Transaction]
loadTransactions tok aid = do
    creds <- Plaid.credentials
    -- TODO how many transactions should we pull?
    -- TODO how far back should we go?
    let options = TransactionsOptions (fromGregorian 2018 09 01) (fromGregorian 2018 12 31) 500 0 [aid]
    res <- runPlaid $ Plaid.reqTransactions creds tok options
    pure $ transactions (res :: TransactionsResponse)
