{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Plaid where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception (Exception)
import Network.Plaid
-- import Network.Plaid.Types ()
import Servant.Client (mkClientEnv, ClientEnv, BaseUrl, ServantError, ClientM)
import Network.HTTP.Client (Manager)

class Monad m => MonadPlaid m where
    credentials :: m Credentials
    manager :: m Manager
    baseUrl :: m BaseUrl


data PlaidError = PlaidError ServantError
    deriving (Show, Eq)

instance Exception PlaidError


runPlaid :: (MonadPlaid m, MonadError PlaidError m, MonadIO m) => ClientM a -> m a
runPlaid req = do
    env <- clientEnv
    res <- liftIO $ runClientM req env
    case res of
      Left err -> throwError $ PlaidError err
      Right a -> pure a


clientEnv :: MonadPlaid m => m ClientEnv
clientEnv = do
    mgr <- manager
    url <- baseUrl
    pure $ mkClientEnv mgr url
