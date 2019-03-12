{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Timely.Transfers.ACH where

import           Control.Monad.Catch    (MonadThrow, MonadCatch)
import           Control.Monad.Config   (MonadConfig (..), configs)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Model.Money       as Money
import           Network.Dwolla         (Amount (..), Customer (..), FundingSource, Id)
import qualified Network.Dwolla         as Dwolla
import           Timely.Transfers.Types




data Config = Config
    { source :: Id FundingSource
    , dwolla :: Dwolla.Config
    }


initAccount :: (MonadThrow m, MonadCatch m, MonadIO m, MonadConfig Config m) => AccountInfo -> m (Id FundingSource)
initAccount account = do
  cfg <- configs dwolla
  tok <- Dwolla.authenticate cfg
  custId <- Dwolla.createCustomer cfg tok (customer account)
  fundId <- Dwolla.createFundingSource cfg tok custId (newFundingSource account)
  pure fundId

  where
    newFundingSource a = Dwolla.CreateFundingSource (processorToken a) "Plaid Checking"







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
  cfg    <- configs dwolla
  tok <- Dwolla.authenticate cfg
  Dwolla.transfer cfg tok src dest (toAmount t)






customer :: AccountInfo -> Customer
customer AccountInfo { firstName, lastName, email } =
  Customer { firstName, lastName, email }



toAmount :: forall a. Transfer a -> Amount
toAmount t = Amount $ Money.toFloat $ amount (t :: Transfer a)


