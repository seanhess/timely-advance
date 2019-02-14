{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Timely.AccountStore.Types where


import           Data.Aeson                (ToJSON(..))
import           Data.Model.Guid           (Guid)
import           Data.Model.Id             (Id (..), Token (..))
import           Data.Model.Money          as Money
import           Data.Model.Types          (Phone, PostalCode, SSN, State)
import           Data.Model.Valid          as Valid
import           Data.Text                 as Text
import           Data.Typeable             (Typeable)
import           Database.Selda            as Selda
import           GHC.Generics              (Generic)
import           Timely.Bank               (Access, Public)
import qualified Timely.Bank               as Bank
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
    , credit     :: Money
    , health     :: Health
    } deriving (Show, Eq, Generic)



data AccountRow = AccountRow
    { accountId  :: Guid Account
    , phone      :: Valid Phone
    , transferId :: Id TransferAccount
    , bankToken  :: Private (Token Access)
    , credit     :: Money
    } deriving (Generic, Eq, Show)

instance SqlRow AccountRow


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
    } deriving (Generic, Show)

instance SqlRow Application


data Onboarding
    = Pending
    | Complete
    | Error
    deriving (Show, Eq, Generic, Enum, Bounded, Read)

instance SqlType Onboarding
instance ToJSON Onboarding
instance ToJSON DenialReason



data AppResult
    = AppResultDenial AppDenial
    | AppResultApproval AppApproval

instance ToJSON AppResult where
  toJSON (AppResultDenial d) = toJSON d
  toJSON (AppResultApproval a) = toJSON a


data AppApproval = AppApproval
    { accountId      :: Guid Account
    , approvalAmount :: Money
    , onboarding     :: Onboarding
    } deriving (Generic, Show)

instance SqlRow AppApproval
instance ToJSON AppApproval


data AppDenial = AppDenial
    { accountId :: Guid Account
    , denial    :: DenialReason
    } deriving (Generic, Show)

instance SqlType DenialReason
instance SqlRow AppDenial
instance ToJSON AppDenial




toBankAccount :: Guid Account -> Bank.Account -> BankAccount
toBankAccount accountId acc = BankAccount {..}
  where
    id = Selda.def
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
    } deriving (Show, Eq, Generic)

instance SqlRow Health
