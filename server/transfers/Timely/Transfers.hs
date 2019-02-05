{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
module Timely.Transfers where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Selda       (Selda, insert, tryCreateTable)
import           Control.Monad.Service     (Service (..))
import           Data.Time.Clock           (UTCTime)
import qualified Data.Time.Clock           as Time
import Data.Typeable (Typeable)
import           Database.Selda            hiding (insert, query, tryCreateTable, update_)
import           GHC.Generics              (Generic)
import           Timely.AccountStore.Types (Account)
import           Timely.Types.Money        (Money)

import           Timely.Types.Guid         (Guid)
import Timely.Advances (Advance(..))



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



data Transfers a where
    Credit :: Advance -> Transfers (Transfer Credit)
    Debit  :: Advance -> Transfers (Transfer Debit)


instance Selda m => Service m Transfers where
  run (Credit a)    = credit a
  run (Debit  a)    = debit a


credits :: Table (Transfer Credit)
credits =
    table "transfers_credits"
      [ #advanceId :- primary ]


debits :: Table (Transfer Debit)
debits =
    table "transfers_debits"
      [ #advanceId :- primary ]


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
