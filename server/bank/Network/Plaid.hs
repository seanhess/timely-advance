{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Network.Plaid
  ( PlaidApi
  , Credentials(..)
  , reqExchangeToken
  , reqAuth
  , reqTransactions
  , reqAccounts
  , runClientM
  , mkClientEnv
  , hoistClient
  ) where


import Data.Proxy (Proxy(..))
import Network.Plaid.Types hiding (accounts)
import Servant ((:>), ReqBody, JSON, Post, (:<|>)(..))
import Servant.Client (ClientM, client, runClientM, mkClientEnv, hoistClient)



-- * Plaid Api Definition
--
-- See https://plaid.com/docs/. Also includes asset reports, getting balances separately

type PlaidApi
  =    "item" :> "public_token" :> "exchange"
          :> ReqBody '[JSON] ExchangeTokenRequest
          :> Post    '[JSON] ExchangeTokenResponse

  :<|> "auth" :> "get"
          :> ReqBody '[JSON] AuthRequest
          :> Post    '[JSON] AuthResponse

  :<|> "transactions" :> "get"
          :> ReqBody '[JSON] TransactionsRequest
          :> Post    '[JSON] TransactionsResponse

  :<|> "accounts" :> "get"
          :> ReqBody '[JSON] AccountsRequest
          :> Post    '[JSON] AccountsResponse




api :: Proxy PlaidApi
api = Proxy


-- * Requests

callExchangeToken :: ExchangeTokenRequest -> ClientM ExchangeTokenResponse
callAuth          :: AuthRequest          -> ClientM AuthResponse
callTransactions  :: TransactionsRequest  -> ClientM TransactionsResponse
callAccounts      :: AccountsRequest      -> ClientM AccountsResponse
callExchangeToken :<|> callAuth :<|> callTransactions :<|> callAccounts = client api



reqExchangeToken :: Credentials -> Token Public -> ClientM ExchangeTokenResponse
reqExchangeToken Credentials {..} public_token = callExchangeToken $ ExchangeTokenRequest {..}


reqAuth :: Credentials -> Token Access -> ClientM AuthResponse
reqAuth Credentials {..} access_token = callAuth $ AuthRequest {..}


reqTransactions :: Credentials -> Token Access -> TransactionsOptions -> ClientM TransactionsResponse
reqTransactions Credentials {..} access_token TransactionsOptions {..} =
    callTransactions $ TransactionsRequest {..}
  where
    options = ListRequestOptions {..}


reqAccounts :: Credentials -> Token Access -> ClientM AccountsResponse
reqAccounts Credentials {..} access_token =
    callAccounts $ AccountsRequest {..}



