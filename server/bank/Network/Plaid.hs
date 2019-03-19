{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DuplicateRecordFields       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TypeOperators   #-}
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


import           Data.Model.Id               (Id (..), Token (..))
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
reqExchangeToken Credentials {client_id, secret} public_token =
  callExchangeToken $ ExchangeToken.Request
    { client_id, secret, public_token }


reqAuth :: Credentials -> Token Access -> ClientM Auth.Response
reqAuth Credentials {client_id, secret} access_token =
  callAuth $ Auth.Request
    { client_id, secret, access_token }


reqTransactions :: Credentials -> Token Access -> Id Account -> Transactions.Options -> ClientM Transactions.Response
reqTransactions Credentials { client_id, secret} access_token accountId Transactions.Options { start_date, end_date, count, offset } =
    callTransactions $ Transactions.Request
      {  client_id, secret, access_token, start_date, end_date, options }
  where
    options = Transactions.ListRequestOptions
      { count, offset, account_ids = [ accountId ] }


reqAccounts :: Credentials -> Token Access -> ClientM Accounts.Response
reqAccounts Credentials { client_id, secret } access_token =
    callAccounts $ Accounts.Request
      { client_id, secret, access_token }


reqIdentity :: Credentials -> Token Access -> ClientM Identity.Response
reqIdentity Credentials { client_id, secret } access_token =
    callIdentity $ Identity.Request
      { client_id, secret, access_token}


reqDwolla :: Credentials -> Token Access -> Id Account -> ClientM Dwolla.Response
reqDwolla Credentials { client_id, secret } access_token account_id =
    callDwolla $ Dwolla.Request
      { client_id, secret, access_token, account_id }
