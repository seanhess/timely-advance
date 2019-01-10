{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Bank
    ( Token
    , Access
    , Public
    , Account(..)
    , Currency(..)
    , Balances(..)
    , AccountType(..)
    , AccountSubType(..)
    , Banks(..)
    ) where

import Control.Monad.Effect (Effect(..))
import Control.Monad.Plaid (MonadPlaid, runPlaid)
import qualified Control.Monad.Plaid as Plaid
import Network.Plaid.Types
import qualified Network.Plaid as Plaid

-- Bank Service

data Banks a where
    Authenticate :: Token Public -> Banks (Token Access)
    LoadAccounts :: Token Access -> Banks [Account]


-- there's an obvious implementation for anyone who has a MonadSelda
instance (MonadPlaid m) => Effect m Banks where
    run (Authenticate t)      = authenticate t
    run (LoadAccounts t)      = loadAccounts t


authenticate :: MonadPlaid m => Token Public -> m (Token Access)
authenticate pub = do
    creds <- Plaid.credentials
    res <- runPlaid $ Plaid.reqExchangeToken creds pub
    pure $ access_token (res :: ExchangeTokenResponse)


loadAccounts :: MonadPlaid m => Token Access -> m [Account]
loadAccounts tok = do
    creds <- Plaid.credentials
    res <- runPlaid $ Plaid.reqAccounts creds tok
    pure $ accounts (res :: AccountsResponse)


