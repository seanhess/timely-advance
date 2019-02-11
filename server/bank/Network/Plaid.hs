{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
module Network.Plaid
  ( PlaidApi
  , Credentials(..)
  , reqExchangeToken
  , reqAuth
  , reqTransactions
  , reqAccounts
  , reqIdentity
  , reqDwolla
  , runClientM
  , mkClientEnv
  , hoistClient
  ) where


import           Data.Proxy                  (Proxy (..))
import qualified Network.Plaid.Accounts      as Accounts
import qualified Network.Plaid.Auth          as Auth
import qualified Network.Plaid.Dwolla        as Dwolla
import qualified Network.Plaid.ExchangeToken as ExchangeToken
import qualified Network.Plaid.Identity      as Identity
import qualified Network.Plaid.Transactions  as Transactions
import           Network.Plaid.Types
import           Servant                     ((:<|>) (..))
import           Servant.Client              (ClientM, client, hoistClient, mkClientEnv, runClientM)



-- * Plaid Api Definition
--
-- See https://plaid.com/docs/. Also includes asset reports, getting balances separately

type PlaidApi
    =  ExchangeToken.Endpoint
  :<|> Auth.Endpoint
  :<|> Transactions.Endpoint
  :<|> Accounts.Endpoint
  :<|> Identity.Endpoint
  :<|> Dwolla.Endpoint




api :: Proxy PlaidApi
api = Proxy


-- * Requests

callExchangeToken :: ExchangeToken.Request -> ClientM ExchangeToken.Response
callAuth          :: Auth.Request          -> ClientM Auth.Response
callTransactions  :: Transactions.Request  -> ClientM Transactions.Response
callAccounts      :: Accounts.Request      -> ClientM Accounts.Response
callIdentity      :: Identity.Request      -> ClientM Identity.Response
callDwolla        :: Dwolla.Request        -> ClientM Dwolla.Response
callExchangeToken :<|> callAuth :<|> callTransactions :<|> callAccounts :<|> callIdentity :<|> callDwolla = client api



reqExchangeToken :: Credentials -> Token Public -> ClientM ExchangeToken.Response
reqExchangeToken Credentials {..} public_token = callExchangeToken $ ExchangeToken.Request {..}


reqAuth :: Credentials -> Token Access -> ClientM Auth.Response
reqAuth Credentials {..} access_token = callAuth $ Auth.Request {..}


reqTransactions :: Credentials -> Token Access -> Transactions.Options -> ClientM Transactions.Response
reqTransactions Credentials {..} access_token Transactions.Options {..} =
    callTransactions $ Transactions.Request {..}
  where
    options = Transactions.ListRequestOptions {..}


reqAccounts :: Credentials -> Token Access -> ClientM Accounts.Response
reqAccounts Credentials {..} access_token =
    callAccounts $ Accounts.Request {..}


reqIdentity :: Credentials -> Token Access -> ClientM Identity.Response
reqIdentity Credentials {..} access_token =
    callIdentity $ Identity.Request {..}


reqDwolla :: Credentials -> Token Access -> Id Account -> ClientM Dwolla.Response
reqDwolla Credentials {..} access_token account_id =
    callDwolla $ Dwolla.Request {..}
