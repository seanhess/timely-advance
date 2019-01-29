{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
module Timely.Bank
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
    , Config(..)
    ) where

import Control.Monad.Service (Service(..))
import Control.Monad.Config (MonadConfig, configs)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception (throw, Exception)
import Network.Plaid.Types
import qualified Network.Plaid as Plaid
import Network.HTTP.Client (Manager)
import Data.Time.Calendar (fromGregorian)
import Servant.Client (ClientM, ClientEnv, runClientM, ServantError, BaseUrl, mkClientEnv)

-- Bank Service

data Banks a where
    Authenticate     :: Token Public -> Banks (Token Access)
    LoadIdentity     :: Token Access -> Banks Identity
    LoadAccounts     :: Token Access -> Banks [Account]
    LoadTransactions :: Token Access -> Id Account -> Banks [Transaction]


-- there's an obvious implementation for anyone who has a MonadSelda
instance (MonadIO m, MonadConfig Config m) => Service m Banks where
    run (Authenticate t)      = authenticate t
    run (LoadIdentity t)      = loadIdentity t
    run (LoadAccounts t)      = loadAccounts t
    run (LoadTransactions t i) = loadTransactions t i


authenticate :: (MonadIO m, MonadConfig Config m) => Token Public -> m (Token Access)
authenticate pub = do
    creds <- configs credentials
    res <- runPlaid $ Plaid.reqExchangeToken creds pub
    pure $ access_token (res :: ExchangeTokenResponse)


loadIdentity :: (MonadIO m, MonadConfig Config m) => Token Access -> m Identity
loadIdentity tok = do
    creds <- configs credentials
    res <- runPlaid $ Plaid.reqIdentity creds tok
    pure $ identity (res :: IdentityResponse)


loadAccounts :: (MonadIO m, MonadConfig Config m) => Token Access -> m [Account]
loadAccounts tok = do
    creds <- configs credentials
    res <- runPlaid $ Plaid.reqAccounts creds tok
    pure $ accounts (res :: AccountsResponse)


-- which transactions should I load? How far back? 3 months?
-- this only works for specific accounts
-- so we don't really care about the account information, do we?
loadTransactions :: (MonadIO m, MonadConfig Config m) => Token Access -> Id Account -> m [Transaction]
loadTransactions tok aid = do
    creds <- configs credentials
    -- TODO how many transactions should we pull?
    -- TODO how far back should we go?
    let options = TransactionsOptions (fromGregorian 2018 09 01) (fromGregorian 2018 12 31) 500 0 [aid]
    res <- runPlaid $ Plaid.reqTransactions creds tok options
    pure $ transactions (res :: TransactionsResponse)



data Config = Config
    { manager :: Manager
    , baseUrl :: BaseUrl
    , credentials :: Credentials
    }

runPlaid :: (MonadIO m, MonadConfig Config m) => ClientM a -> m a
runPlaid req = do
    env <- clientEnv
    res <- liftIO $ runClientM req env
    case res of
      Left err -> throw $ PlaidError err
      Right a -> pure a


clientEnv :: MonadConfig Config m => m ClientEnv
clientEnv = do
    mgr <- configs manager
    url <- configs baseUrl
    pure $ mkClientEnv mgr url




data PlaidError = PlaidError ServantError
    deriving (Show, Eq)

instance Exception PlaidError
