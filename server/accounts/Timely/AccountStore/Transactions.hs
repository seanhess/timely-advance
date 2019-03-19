{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLabels          #-}
module Timely.AccountStore.Transactions where



import           Control.Effects           (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import           Control.Monad.Selda       (Selda, insert, tryCreateTable)
import           Data.Model.Guid           (Guid)
import           Data.Model.Id             (Id (..))
import           Data.Model.Money          (Money)
import qualified Data.Model.Money          as Money
import qualified Data.Text as Text
import           Data.Proxy
import           Database.Selda            hiding (deleteFrom, insert, query, tryCreateTable)
import           Database.Selda.SqlType    (Lit (..), SqlType (..))
import           GHC.Generics              (Generic)
import           Timely.AccountStore.Types (Account)
import Timely.AccountStore.Account (accounts)
import qualified Timely.Bank               as Bank (Transaction(..), Currency(..), Category(..))






data Transactions m = TransactionsMethods
  { _save         :: Guid Account -> [Transaction] -> m ()
  } deriving (Generic)

instance Effect Transactions


save     :: MonadEffect Transactions m => Guid Account -> [Transaction] -> m ()
TransactionsMethods save = effect




implementIO :: Selda m => RuntimeImplemented Transactions m a -> m a
implementIO = implement $
  TransactionsMethods
    saveTransactions



-- Types ---------------------------------------------

data Transaction = Transaction
  { transactionId :: Id Transaction
  , accountId     :: Guid Account
  , date          :: Day
  , category      :: Category
  , pending       :: Bool
  , amount        :: Money
  , name          :: Text
  } deriving (Show, Eq, Generic)

instance SqlRow Transaction






-- Selda implementation ------------------------------



transactions :: Table Transaction
transactions = table "accounts_transactions"
  [ #transactionId :- primary
  , #accountId :- foreignKey accounts #accountId
  ]



-- | This will fail if any of the transactionIds already exists. That's kind of stupid
saveTransactions :: Selda m => Guid Account -> [Transaction] -> m ()
saveTransactions _ ts = do
    insert transactions ts
    pure ()




initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable transactions






-- More Types ---------------------------------------------

fromBank :: Guid Account -> Bank.Transaction -> Transaction
fromBank accountId t =
  let Id id = Bank.transaction_id t
      Bank.Currency a = Bank.amount t
  in Transaction
    { transactionId = Id id
    , accountId = accountId
    , date = Bank.date t
    , category = Category $ cats (Bank.category t)
    , amount = Money.fromFloat a
    , pending = Bank.pending t
    , name = Bank.name t
    }
  where
   -- TODO handle unknown categories. How do we select between them?
   cats (Just cs) = Text.intercalate " | " $ map cat cs
   cats _ = ""
   cat (Bank.Category c) = c












newtype Category = Category Text
  deriving (Show, Eq, Generic)

instance SqlType Category where
    mkLit (Category b) =  LCustom $ mkLit b
    sqlType _ = sqlType (Proxy :: Proxy Text)
    fromSql v = Category $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Text)
