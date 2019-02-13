{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE FlexibleContexts   #-}
module Timely.Transfers.ACH where

import           Control.Monad.Catch       (MonadThrow, throwM)
import           Control.Monad.Config      (MonadConfig (..), configs)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Model.Money          as Money
import           Data.Model.Types          (Address (..), Valid (..))
import           Network.Dwolla            (Credentials, Customer (..), DwollaError (..), FundingSource, Id,
                                            Static (..), Amount(..))
import qualified Network.Dwolla            as Dwolla
import qualified Network.Dwolla.Types      as Dwolla
import           Network.HTTP.Client       (Manager)
import           Servant.Client            (BaseUrl, ClientEnv, ClientM, mkClientEnv, runClientM)
import           Timely.Transfers.Types




initAccount :: (MonadThrow m, MonadIO m, MonadConfig Config m) => AccountInfo -> m (Id FundingSource)
initAccount account = do
  creds <- configs credentials
  fundId <- runDwolla $ do
    tok    <- Dwolla.authenticate creds
    custId <- Dwolla.createCustomer tok (customer account)
    fundId <- Dwolla.createFundingSource tok custId (fundingSource account)
    pure $ fundId
  pure fundId
  -- createTransferAccount (accountId (account :: AccountInfo)) fundId

  where
    fundingSource a = Dwolla.FundingSource (processorToken a) "Plaid Checking"







-- Send ACH Transfers ----------------------------------------

sendMoney :: (MonadThrow m, MonadConfig Config m, MonadIO m) => Id FundingSource -> Transfer Credit -> m ()
sendMoney cust t = do
  timely <- configs source
  transferMoney timely cust t
  pure ()


collectMoney :: (MonadThrow m, MonadConfig Config m, MonadIO m) => Id FundingSource -> Transfer Debit -> m ()
collectMoney cust t = do
  timely <- configs source
  transferMoney cust timely t
  pure ()



transferMoney :: (MonadThrow m, MonadConfig Config m, MonadIO m) => Id FundingSource -> Id FundingSource -> Transfer a -> m (Id Dwolla.Transfer)
transferMoney src dest t = do
  url    <- configs baseUrl
  creds  <- configs credentials
  runDwolla $ do
    tok <- Dwolla.authenticate creds
    Dwolla.transfer tok (Dwolla.fundingSource url src) (Dwolla.fundingSource url dest) (toAmount t)






customer :: AccountInfo -> Customer
customer AccountInfo { firstName, lastName, email, address, dateOfBirth, ssn } =
  let Address { street1, street2, city, state, postalCode } = address
  in Customer
    { firstName, lastName, email
    , ipAddress = Nothing
    , type_ = Static
    , address1    = Dwolla.Address street1
    , address2    = Dwolla.Address <$> street2
    , city
    , state       = valid state
    , postalCode  = valid postalCode
    , dateOfBirth
    , ssn         = Dwolla.last4SSN $ valid ssn
    , phone       = Nothing
    }



toAmount :: forall a. Transfer a -> Amount
toAmount t = Amount $ Money.toFloat $ amount (t :: Transfer a)




-- Dwolla API Helpers ----------------------------------------------

data Config = Config
    { manager     :: Manager
    , baseUrl     :: BaseUrl
    , credentials :: Credentials
    , source      :: Id FundingSource
    }


runDwolla :: (MonadThrow m, MonadIO m, MonadConfig Config m) => ClientM a -> m a
runDwolla req = do
    env <- clientEnv
    res <- liftIO $ runClientM req env
    case res of
      Left err -> throwM $ DwollaApiError err
      Right a  -> pure a


clientEnv :: MonadConfig Config m => m ClientEnv
clientEnv = do
    mgr <- configs manager
    url <- configs baseUrl
    pure $ mkClientEnv mgr url


