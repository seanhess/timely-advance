{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
module Timely.Accounts.Transactions where



import Control.Monad.Selda     (Selda, deleteFrom, insert, query, tryCreateTable)
import Data.Model.Guid         (Guid)
import Database.Selda          hiding (deleteFrom, insert, query, tryCreateTable)
import Timely.Accounts.Account (accounts)
import Timely.Accounts.Types   (Account, TransactionRow (..))






type Count = Int
type Offset = Int



-- Selda implementation ------------------------------



transactions :: Table TransactionRow
transactions = table "accounts_transactions"
  [ #transactionId :- primary
  , #accountId     :- foreignKey accounts #accountId
  ]



-- | This will fail if any of the transactionIds already exists. That's kind of stupid
save :: Selda m => Guid Account -> [TransactionRow] -> m ()
save _ ts = do
    insert transactions ts
    pure ()


selectAccount :: Guid Account -> Query s (Row s TransactionRow)
selectAccount i = do
  t <- select transactions
  restrict (t ! #accountId .== literal i)
  order    (t ! #date) descending
  pure t



list :: Selda m => Guid Account -> Offset -> Count -> m [TransactionRow]
list i offset count = do
  query $ limit offset count $ selectAccount i


since :: Selda m => Guid Account -> Day -> m [TransactionRow]
since i day =
  query $ do
    t <- selectAccount i
    restrict (t ! #date .>= literal day)
    pure t





initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable transactions


deleteAccount :: (Selda m, MonadIO m) => Guid Account -> m ()
deleteAccount i = do
  deleteFrom transactions (\x -> x ! #accountId .== literal i)
  pure ()















