{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Timely.AccountStore.Types where


import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Char                 as Char
import           Data.Proxy                (Proxy (..))
import           Data.Text                 as Text
import           Data.Typeable             (Typeable)
import           Database.Selda            as Selda
import           Database.Selda.SqlType    (Lit (..))
import           GHC.Generics              (Generic)
import           Data.Model.Guid           (Guid)
import           Data.Model.Money          as Money
import           Data.Model.Id             (Token(..), Id(..))
import           Timely.Auth               (Phone)
import           Timely.Bank               (Access, Public)
import qualified Timely.Bank               as Bank
import           Timely.Types.Private
import           Timely.Underwriting.Types (DenialReason)



-- aggregate all account information
data Account = Account
    { accountId :: Guid Account
    , phone     :: Phone
    , customer  :: Customer
    , bankToken :: Private (Token Access)
    , credit    :: Money
    , health    :: Health
    } deriving (Show, Eq, Generic)



data AccountRow = AccountRow
    { accountId :: Guid Account
    , phone     :: Phone
    , bankToken :: Private (Token Access)
    , credit    :: Money
    } deriving (Generic, Eq, Show)

instance SqlRow AccountRow


data Customer = Customer
    { id          :: ID Customer
    , accountId   :: Guid Account
    , firstName   :: Text
    , middleName  :: Maybe Text
    , lastName    :: Text
    , email       :: Text
    , ssn         :: Digits SSN
    , dateOfBirth :: Day
    } deriving (Generic, Eq, Show)

instance SqlRow Customer


-- only digits
data SSN
newtype Digits a = Digits Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


digits :: Text -> Digits a
digits = Digits . Text.filter Char.isDigit

instance Typeable t => SqlType (Digits t) where
    mkLit (Digits t) =  LCustom $ mkLit t
    sqlType _ = sqlType (Proxy :: Proxy Text)
    fromSql v = Digits $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Text)


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
    , phone           :: Phone
    , email           :: Text
    , ssn             :: Digits SSN
    , dateOfBirth     :: Day
    , publicBankToken :: Token Public
    } deriving (Generic, Show)

instance SqlRow Application


data AppApproval = AppApproval
    { accountId      :: Guid Account
    , approvalAmount :: Money
    } deriving (Generic, Show)

instance SqlRow AppApproval


data AppDenial = AppDenial
    { accountId :: Guid Account
    , denial    :: DenialReason
    } deriving (Generic, Show)

instance SqlType DenialReason
instance SqlRow AppDenial




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
