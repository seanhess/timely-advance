{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Timely.Transfers where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Selda       (Selda, insert, tryCreateTable)
import           Control.Monad.Service     (Service (..))
import           Data.Model.Guid           (Guid)
import           Data.Model.Id             (Token)
import           Data.Model.Money          (Money)
import           Data.Time.Clock           (UTCTime)
import qualified Data.Time.Clock           as Time
import           Data.Typeable             (Typeable)
import           Database.Selda            hiding (insert, query, tryCreateTable, update_)
import           GHC.Generics              (Generic)
import           Network.Dwolla            (FundingSource, Id)
import           Network.Plaid.Dwolla      (Dwolla)
import           Timely.AccountStore.Types (Account, Digits, SSN)

import           Timely.Advances           (Advance (..))



-- there's only one transfer per advance... for now
data Transfer a = Transfer
    { advanceId :: Guid Advance
    , accountId :: Guid Account
    , amount    :: Money
    , created   :: UTCTime
    , success   :: Maybe UTCTime
    } deriving (Show, Eq, Generic)

instance Typeable a => SqlRow (Transfer a)


data Credit
data Debit


data AccountInfo = AccountInfo
  { accountId   :: Guid Account
  , firstName   :: Text
  , lastName    :: Text
  , email       :: Text
  , address1    :: Text
  , address2    :: Maybe Text
  , city        :: Text
  , state       :: Text
  , postalCode  :: Text
  , dateOfBirth :: Day
  , ssn         :: Digits SSN
  , plaidToken  :: Token Dwolla
  } deriving (Show, Eq, Generic)



data TransferAccount = TransferAccount
  { accountId     :: Guid Account
  , fundingSource :: Id FundingSource
  } deriving (Show, Eq, Generic)

instance SqlRow TransferAccount


data Transfers a where
    Credit :: Advance -> Transfers (Transfer Credit)
    Debit  :: Advance -> Transfers (Transfer Debit)

    -- this creates the funding source, etc, and saves the information for later
    Init   :: AccountInfo -> Transfers ()


instance Selda m => Service m Transfers where
  run (Credit a) = credit a
  run (Debit  a) = debit a

  run (Init _)   = undefined -- init i


credits :: Table (Transfer Credit)
credits =
    table "transfers_credits"
      [ #advanceId :- primary ]


debits :: Table (Transfer Debit)
debits =
    table "transfers_debits"
      [ #advanceId :- primary ]


accounts :: Table (Transfer Debit)
accounts =
    table "transfers_accounts"
      [ #accountId :- primary ]


credit :: Selda m => Advance -> m (Transfer Credit)
credit adv = do
    credit <- transfer adv
    insert credits [credit]
    pure credit


debit :: Selda m => Advance -> m (Transfer Debit)
debit adv = do
    debit <- transfer adv
    insert debits [debit]
    pure debit


transfer :: MonadIO m => Advance -> m (Transfer a)
transfer Advance {..} = do
    created <- liftIO $ Time.getCurrentTime
    let success = Nothing
    pure Transfer {..}




initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable credits
    tryCreateTable debits
    tryCreateTable accounts
