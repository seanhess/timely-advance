{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
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
    , loadIdentity -- remove me when you add it back in
    ) where

import           Control.Exception      (Exception, throw)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.Config   (MonadConfig, configs)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Service  (Service (..))
import           Data.List              as List
import           Data.Text              as Text
import           Data.Time.Calendar     (fromGregorian)
import           Network.HTTP.Client    (Manager)
import qualified Network.Plaid          as Plaid
import           Network.Plaid.Types    hiding (Identity)
import qualified Network.Plaid.Types    as Plaid
import           Servant.Client         (BaseUrl, ClientEnv, ClientM, ServantError, mkClientEnv, runClientM)

-- Bank Service

data Banks a where
    Authenticate     :: Token Public -> Banks (Token Access)
    LoadIdentity     :: Token Access -> Banks Identity
    LoadAccounts     :: Token Access -> Banks [Account]
    LoadTransactions :: Token Access -> Id Account -> Banks [Transaction]


instance (MonadIO m, MonadThrow m, MonadConfig Config m) => Service m Banks where
    run (Authenticate t)       = authenticate t
    run (LoadAccounts t)       = loadAccounts t
    run (LoadTransactions t i) = loadTransactions t i
    -- run (LoadIdentity t)      = loadIdentity t
    run (LoadIdentity _)       = pure $ Identity "Mock" Nothing "Person"


authenticate :: (MonadIO m, MonadConfig Config m) => Token Public -> m (Token Access)
authenticate pub = do
    creds <- configs credentials
    res <- runPlaid $ Plaid.reqExchangeToken creds pub
    pure $ access_token (res :: ExchangeTokenResponse)


loadIdentity :: (MonadIO m, MonadConfig Config m, MonadThrow m) => Token Access -> m Identity
loadIdentity tok = do
    creds <- configs credentials
    res <- runPlaid $ Plaid.reqIdentity creds tok
    parseIdentity $ identity (res :: IdentityResponse)



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
    { manager     :: Manager
    , baseUrl     :: BaseUrl
    , credentials :: Credentials
    }

runPlaid :: (MonadIO m, MonadConfig Config m) => ClientM a -> m a
runPlaid req = do
    env <- clientEnv
    res <- liftIO $ runClientM req env
    case res of
      Left err -> throw $ PlaidError err
      Right a  -> pure a


clientEnv :: MonadConfig Config m => m ClientEnv
clientEnv = do
    mgr <- configs manager
    url <- configs baseUrl
    pure $ mkClientEnv mgr url




data BankError
    = BadName Text
    | NoNames
    | PlaidError ServantError
    deriving (Eq, Show)

instance Exception BankError



data Identity = Identity
    { firstName  :: Text
    , middleName :: Maybe Text
    , lastName   :: Text
    } deriving (Show, Eq)


parseIdentity :: MonadThrow m => Plaid.Identity -> m Identity
parseIdentity identity =
  case List.map Text.words (Plaid.names identity) of
    [f, m, l]:_ -> pure $ Identity f (Just m) l
    [f, l]:_    -> pure $ Identity f Nothing l
    n:_         -> throwM $ BadName $ Text.unwords n
    _           -> throwM $ NoNames
