{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
module Timely.Accounts.Transactions where



import           Control.Monad.Selda     (Selda, insert, query, tryCreateTable)
import           Data.Model.Guid         (Guid)
import           Database.Selda          hiding (deleteFrom, insert, query, tryCreateTable)
import           Timely.Accounts.Account (accounts)
import           Timely.Accounts.Types   (Account, TransactionRow(..))






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


list :: Selda m => Guid Account -> Offset -> Count -> m [TransactionRow]
list i offset count = do
  ts <- query $ limit offset count $ do
    t <- select transactions
    restrict (t ! #accountId .== literal i)
    order    (t ! #date) descending
    pure t
  liftIO $ mapM print ts
  pure ts




initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable transactions















