{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Timely.AccountStore.Account where


import           Control.Effects           (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import           Control.Monad.Selda       (Selda, deleteFrom, insert, query, tryCreateTable)
import           Data.Maybe                (listToMaybe)
import           Data.Model.Guid
import           Data.Model.Id             (Id)
import           Data.Model.Money
import           Data.Model.Types          (Phone)
import           Data.Model.Valid          (Valid)
import qualified Data.Time.Clock           as Time
import           Database.Selda            hiding (deleteFrom, insert, query, tryCreateTable)
import           GHC.Generics              (Generic)

import           Timely.AccountStore.Types
import           Timely.Bank               (Access, Token)
import           Timely.Evaluate.Types     (Projection (..))
import           Timely.Transfers.Account  (TransferAccount)
import           Timely.Types.Private




data Accounts m = AccountsMethods
    { _all         :: m [AccountRow]
    , _find        :: Guid Account -> m (Maybe Account)
    , _findByPhone :: Valid Phone -> m (Maybe (Guid Account))
    , _create      :: Account -> m ()
    , _setHealth   :: Guid Account -> Projection -> m ()
    , _findBanks   :: Guid Account -> m [BankAccount]
    , _setBanks    :: Guid Account -> [BankAccount] -> m ()
    } deriving (Generic)

instance Effect Accounts

all         :: MonadEffect Accounts m => m [AccountRow]
find        :: MonadEffect Accounts m => Guid Account -> m (Maybe Account)
findByPhone :: MonadEffect Accounts m => Valid Phone -> m (Maybe (Guid Account))
findBanks   :: MonadEffect Accounts m => Guid Account -> m [BankAccount]
setHealth   :: MonadEffect Accounts m => Guid Account -> Projection -> m ()
create      :: MonadEffect Accounts m => Account -> m ()
setBanks    :: MonadEffect Accounts m => Guid Account -> [BankAccount] -> m ()
AccountsMethods all find findByPhone create setHealth findBanks setBanks = effect



implementIO :: Selda m => RuntimeImplemented Accounts m a -> m a
implementIO = implement $
  AccountsMethods
    allAccounts
    getAccount
    getAccountIdByPhone
    createAccount
    setAccountHealth
    getBankAccounts
    setBankAccounts



accounts :: Table AccountRow
accounts = table "accounts"
    [ #accountId :- primary
    , #phone :- index
    , #phone :- unique
    ]

customers :: Table Customer
customers = table "accounts_customers" [#id :- autoPrimary, #accountId :- foreignKey accounts #accountId ]

banks :: Table BankAccount
banks = table "accounts_banks" [#id :- autoPrimary, #accountId :- foreignKey accounts #accountId]

healths :: Table Health
healths = table "accounts_healths"
    [ #accountId :- primary
    , #accountId :- foreignKey accounts #accountId
    ]


allAccounts :: (Selda m) => m [AccountRow]
allAccounts =
    query $ select accounts



getAccount :: Selda m => Guid Account -> m (Maybe Account)
getAccount i = do
    as <- query $ do
      c <- select customers
      a <- select accounts
      h <- select healths
      restrict (a ! #accountId .== literal i)
      restrict (c ! #accountId .== literal i)
      restrict (h ! #accountId .== literal i)
      pure (a :*: c :*: h)
    pure $ account <$> listToMaybe as
  where
    account (AccountRow {accountId, phone, transferId, bankToken, credit, created} :*: customer :*: health) =
      Account
        { accountId
        , phone
        , customer
        , transferId
        , bankToken
        , credit
        , health
        , created
        }


getAccountIdByPhone :: Selda m => Valid Phone -> m (Maybe (Guid Account))
getAccountIdByPhone p = do
    as <- query $ do
      a <- select accounts
      restrict (a ! #phone .== literal p)
      pure $ a ! #accountId
    pure $ listToMaybe as




getBankAccounts :: Selda m => Guid Account -> m [BankAccount]
getBankAccounts i =
    query $ do
      b <- select banks
      restrict (b ! #accountId .== literal i)
      pure b



setAccountHealth :: Selda m => Guid Account -> Projection -> m ()
setAccountHealth i Projection {expenses, available} = do
    now <- liftIO $ Time.getCurrentTime
    deleteFrom healths (\h -> h ! #accountId .== literal i)
    insert healths [Health {accountId = i, expenses, available, created = now }]
    pure ()


createAccount :: Selda m => Account -> m ()
createAccount acc = do
    -- accounts must be first!
    insert accounts [accountRow acc]
    insert customers [customer acc]
    insert healths [health acc]
    pure ()


account :: Guid Account -> UTCTime -> Valid Phone -> Customer -> Token Access -> Money -> Projection -> Id TransferAccount -> Account
account accountId now phone customer tok credit Projection {expenses, available} transferId =
  Account
    { accountId
    , phone
    , customer
    , transferId
    , bankToken = Private tok
    , credit
    , created = now
    , health = Health { accountId, expenses, available, created  = now}
    }



accountRow :: Account -> AccountRow
accountRow Account { accountId, phone, transferId, bankToken, credit, created } =
  AccountRow { accountId, phone, transferId, bankToken, credit, created }



setBankAccounts :: Selda m => Guid Account -> [BankAccount] -> m ()
setBankAccounts i bs = do
    deleteFrom banks (\b -> b ! #accountId .== literal i)
    insert banks bs
    pure ()



initialize :: (Selda m, MonadIO m) => m ()
initialize = do
    -- drop the table / db first to run migrations
    tryCreateTable accounts
    tryCreateTable customers
    tryCreateTable banks
    tryCreateTable healths
