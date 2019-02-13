{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Timely.Transfers.Types where

import           Control.Exception         (Exception)
import           Data.Model.Guid           (Guid)
import           Data.Model.Id             (Token)
import           Data.Model.Money          as Money
import           Data.Model.Types          (Address (..), SSN, Valid (..))
import           Data.Typeable             (Typeable)
import           Database.Selda            hiding (insert, query, tryCreateTable, update_)
import           GHC.Generics              (Generic)
import           Network.Plaid.Dwolla      (Dwolla)
import           Timely.AccountStore.Types (Account)

import           Timely.Advances           (Advance (..))



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
  , address     :: Address
  , dateOfBirth :: Day
  , ssn         :: Valid SSN
  , processorToken :: Token Dwolla
  } deriving (Show, Eq, Generic)




-- Errors ------------------------------

data TransferError
  = AccountNotInitialized
  deriving (Show, Eq)
instance Exception TransferError
