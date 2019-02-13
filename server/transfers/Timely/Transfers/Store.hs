{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Timely.Transfers.Store where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Selda    (Selda, insert, tryCreateTable, update_)
import           Data.Proxy             (Proxy (..))
import qualified Data.Time.Clock        as Time
import           Data.Typeable          (Typeable)
import           Database.Selda         hiding (insert, query, tryCreateTable, update_)
import           Timely.Advances        (Advance (..))
import           Timely.Transfers.Types


credits :: Table (Transfer Credit)
credits =
    table "transfers_credits"
      [ #advanceId :- primary ]


debits :: Table (Transfer Debit)
debits =
    table "transfers_debits"
      [ #advanceId :- primary ]



class TransferStore a where
  transferTable :: Proxy a -> Table (Transfer a)

instance TransferStore Credit where
  transferTable _ = credits

instance TransferStore Debit where
  transferTable _ = debits


-- database -----------------------------------------------


saveCredit :: Selda m => Advance -> m (Transfer Credit)
saveCredit = saveTransfer credits


saveDebit :: Selda m => Advance -> m (Transfer Debit)
saveDebit = saveTransfer debits


saveTransfer :: (Typeable a, Selda m) => Table (Transfer a) -> Advance -> m (Transfer a)
saveTransfer table adv = do
    trans <- transfer adv
    insert table [trans]
    pure trans


markSuccess :: forall a m. (Typeable a, TransferStore a, Selda m) => Transfer a -> m ()
markSuccess Transfer {advanceId} = do
    let table = transferTable $ Proxy @a
    time <- liftIO $ Time.getCurrentTime
    update_ table (\t -> t ! #advanceId .== literal advanceId)
                 (\t -> t `with` [#success := just (literal time)])


transfer :: MonadIO m => Advance -> m (Transfer a)
transfer Advance { advanceId, accountId, amount } = do
    created <- liftIO $ Time.getCurrentTime
    pure $ Transfer
      { advanceId, accountId, amount, created, success = Nothing }





initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable credits
    tryCreateTable debits
