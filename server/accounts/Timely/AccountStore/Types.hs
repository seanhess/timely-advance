{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Timely.AccountStore.Types where


import           Data.Aeson                (FromJSON (..), ToJSON (..))
import           Data.Model.Guid           (Guid, GuidPrefix (..))
import           Data.Model.Id             (Id (..), Token (..))
import           Data.Model.Money          as Money
import           Data.Model.Types          (Phone, PostalCode, SSN, State)
import           Data.Model.Valid          as Valid
import           Data.Text                 as Text
import           Data.Typeable             (Typeable)
import           Database.Selda            as Selda
import           GHC.Generics              (Generic)
import           Timely.Bank               (Access, Item, Public)
import qualified Timely.Bank               as Bank (Account (..), AccountSubType (..), AccountType (..), Balances (..))
import qualified Timely.Bank               as Bank (Currency (..))
import           Timely.Transfers.Account  (TransferAccount)
import           Timely.Types.Private
import           Timely.Underwriting.Types (DenialReason)



-- aggregate all account information
data Account = Account
    { accountId  :: Guid Account
    , phone      :: Valid Phone
    , customer   :: Customer
    , transferId :: Id TransferAccount
    , bankToken  :: Private (Token Access)
    , bankItemId :: Id Item
    , credit     :: Money
    , health     :: Health
    , created    :: UTCTime
    } deriving (Show, Eq, Generic)

instance GuidPrefix Account where
  guidPrefix _ = "acc"



data AccountRow = AccountRow
    { accountId  :: Guid Account
    , phone      :: Valid Phone
    , transferId :: Id TransferAccount
    , bankToken  :: Token Access
    , bankItemId :: Id Item
    , credit     :: Money
    , created    :: UTCTime
    } deriving (Generic, Eq, Show)

instance SqlRow AccountRow
instance ToJSON AccountRow
instance FromJSON AccountRow


data Customer = Customer
    { id          :: ID Customer
    , accountId   :: Guid Account
    , firstName   :: Text
    , middleName  :: Maybe Text
    , lastName    :: Text
    , email       :: Text
    , ssn         :: Valid SSN
    , dateOfBirth :: Day
    , street1     :: Text
    , street2     :: Maybe Text
    , city        :: Text
    , state       :: Valid State
    , postalCode  :: Valid PostalCode
    , created     :: UTCTime
    } deriving (Generic, Eq, Show)

instance SqlRow Customer





data BankAccountType
    = Checking
    | Savings
    | Credit
    | Other
    deriving (Generic, Eq, Show, Bounded, Enum, Read, Typeable)

instance SqlType BankAccountType



data BankAccount = BankAccount
    { id            :: ID BankAccount
    , accountId     :: Guid Account
    , accountType   :: BankAccountType
    , bankAccountId :: Id Bank.Account
    , name          :: Text
    , balance       :: Money
    , created       :: UTCTime
    } deriving (Generic, Eq, Show)


instance SqlRow BankAccount




-- Can we have more than one application per phone number? NO
data Application = Application
    { accountId       :: Guid Account
    , phone           :: Valid Phone
    , email           :: Text
    , ssn             :: Valid SSN
    , dateOfBirth     :: Day
    , publicBankToken :: Token Public
    , created         :: UTCTime
    , onboarding      :: Onboarding
    } deriving (Generic, Show)

instance SqlRow Application
instance ToJSON Application
instance FromJSON Application


-- I need to associate a bank id with the account
-- just update it in place?

data Onboarding
    = Pending
    | Complete
    | Error
    deriving (Show, Eq, Generic, Enum, Bounded, Read)

instance SqlType Onboarding
instance ToJSON Onboarding
instance FromJSON Onboarding
instance ToJSON DenialReason



data AppResult
    = AppResultDenial AppDenial
    | AppResultApproval AppApproval

instance ToJSON AppResult where
  toJSON (AppResultDenial d)   = toJSON d
  toJSON (AppResultApproval a) = toJSON a


data AppApproval = AppApproval
    { accountId      :: Guid Account
    , approvalAmount :: Money
    , created        :: UTCTime
    } deriving (Generic, Show)

instance SqlRow AppApproval
instance ToJSON AppApproval


data AppDenial = AppDenial
    { accountId :: Guid Account
    , denial    :: DenialReason
    , created   :: UTCTime
    } deriving (Generic, Show)

instance SqlType DenialReason
instance SqlRow AppDenial
instance ToJSON AppDenial



-- save this as soon as we have the bank id
data AppBank = AppBank
    { accountId    :: Guid Account
    , bankItemId   :: Id Item
    , transactions :: Maybe Int
    } deriving (Generic, Show)

instance SqlRow AppBank



toBankAccount :: Guid Account -> UTCTime -> Bank.Account -> BankAccount
toBankAccount accountId now acc = BankAccount {..}
  where
    id = Selda.def
    created = now
    accountType
      | Bank.subtype acc == Bank.Checking = Checking
      | Bank.subtype acc == Bank.Savings = Savings
      | Bank._type acc == Bank.Credit = Credit
      | Bank._type acc == Bank.Depository = Savings
      | Bank._type acc == Bank.Loan = Credit
      | otherwise = Other
    name = Bank.name acc
    balance = toBalance $ Bank.current $ Bank.balances acc
    toBalance (Bank.Currency d) = Money.fromFloat d
    bankAccountId = Bank.account_id acc


isChecking :: BankAccount -> Bool
isChecking acc = accountType acc == Checking




-- Account Projection -------------------


data Health = Health
    { accountId :: Guid Account
    , expenses  :: Money
    , available :: Money
    , created   :: UTCTime
    } deriving (Show, Eq, Generic)

instance SqlRow Health
