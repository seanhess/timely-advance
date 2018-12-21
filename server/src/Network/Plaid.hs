{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Network.Plaid
  ( PlaidApi
  , sendExchangeToken
  , sendAuth
  , sendTransactions
  , sendAccounts
  , runClientM
  , mkClientEnv
  , hoistClient
  ) where


import Data.Proxy (Proxy(..))
import Network.Plaid.Types
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

sendExchangeToken :: ExchangeTokenRequest -> ClientM ExchangeTokenResponse
sendAuth          :: AuthRequest          -> ClientM AuthResponse
sendTransactions  :: TransactionsRequest  -> ClientM TransactionsResponse
sendAccounts      :: AccountsRequest      -> ClientM AccountsResponse
sendExchangeToken :<|> sendAuth :<|> sendTransactions :<|> sendAccounts = client api



