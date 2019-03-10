{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Timely.Bank.Actions where

import           Control.Applicative         ((<|>))
import           Control.Exception           (throw)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.Config        (MonadConfig, configs)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.List                   as List
import qualified Data.Maybe                  as Maybe
import           Data.Model.Id               (Id (..), Token (..))
import           Data.Model.Types            (Address (..), Validate (..))
import           Data.Text                   as Text
import           Data.Time.Calendar          (fromGregorian)
import qualified Network.Plaid               as Plaid
import qualified Network.Plaid.Accounts      as Accounts
import           Network.Plaid.Dwolla        (Dwolla)
import qualified Network.Plaid.Dwolla        as Dwolla
import qualified Network.Plaid.ExchangeToken as ExchangeToken
import           Network.Plaid.Identity      (AddressInfo (..))
import qualified Network.Plaid.Identity      as Identity
import qualified Network.Plaid.Transactions  as Transactions
import           Network.Plaid.Types         (Access, Account, Public, Transaction)
import           Servant.Client              (ClientEnv, ClientM, mkClientEnv, runClientM)
import           Timely.Bank.Types           (BankError (..), Config (..), Identity (..), Names (..))



authenticate :: (MonadIO m, MonadConfig Config m) => Token Public -> m (Token Access)
authenticate pub = do
    creds <- configs credentials
    res <- runPlaid $ Plaid.reqExchangeToken creds pub
    pure $ ExchangeToken.access_token res


loadIdentity :: (MonadIO m, MonadConfig Config m, MonadThrow m) => Token Access -> m Identity
loadIdentity tok = do
    creds <- configs credentials
    res <- runPlaid $ Plaid.reqIdentity creds tok
    names <- parseNames $ Identity.identity res
    address <- parseAddress $ Identity.identity res
    pure $ Identity names address



loadAccounts :: (MonadIO m, MonadConfig Config m) => Token Access -> m [Account]
loadAccounts tok = do
    creds <- configs credentials
    res <- runPlaid $ Plaid.reqAccounts creds tok
    pure $ Accounts.accounts res


-- which transactions should I load? How far back? 3 months?
-- this only works for specific accounts
-- so we don't really care about the account information, do we?
loadTransactions :: (MonadIO m, MonadConfig Config m) => Token Access -> Id Account -> m [Transaction]
loadTransactions tok aid = do
    creds <- configs credentials
    -- TODO how many transactions should we pull?
    -- TODO how far back should we go?
    let options = Transactions.Options (fromGregorian 2018 09 01) (fromGregorian 2018 12 31) 500 0 [aid]
    res <- runPlaid $ Plaid.reqTransactions creds tok options
    pure $ Transactions.transactions res


getACH :: (MonadIO m, MonadConfig Config m) => Token Access -> Id Account -> m (Token Dwolla)
getACH tok aid = do
    creds <- configs credentials
    res <- runPlaid $ Plaid.reqDwolla creds tok aid
    pure $ Dwolla.processor_token res



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




parseNames :: MonadThrow m => Identity.Identity -> m Names
parseNames identity =
  case List.map Text.words (Identity.names identity) of
    [f, m, l]:_ -> pure $ Names f (Just m) l
    [f, l]:_    -> pure $ Names f Nothing l
    n:_         -> throwM $ BadName $ Text.unwords n
    _           -> throwM $ NoNames


parseAddress :: MonadThrow m => Identity.Identity -> m Address
parseAddress identity = do
  let addr = do info <- findBest (Identity.addresses identity)
                toAddress info
  case addr of
    Nothing -> throwM $ BadAddresses $ Identity.addresses identity
    Just a  -> pure a



  where
    findBest :: [AddressInfo] -> Maybe AddressInfo
    findBest as = List.find isPrimary as <|> Maybe.listToMaybe as

    isPrimary :: AddressInfo -> Bool
    isPrimary AddressInfo {_primary} = _primary

    toAddress :: AddressInfo -> Maybe Address
    toAddress AddressInfo {_data} = do
      let Identity.Address {street, city, state, zip} = _data
      st <- validate state
      pc <- validate zip
      pure $ Address
        { street1 = street
        , street2 = Nothing
        , city
        , state = st
        , postalCode = pc
        }
