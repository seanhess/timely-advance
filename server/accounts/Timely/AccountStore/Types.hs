{-# LANGUAGE DuplicateRecordFields, DeriveGeneric #-}
module Timely.AccountStore.Types where


import Database.Selda as Selda
import Database.Selda.SqlType (Lit(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import           Timely.Bank (Token(..), Public, Access, Id(..))
import qualified Timely.Bank as Bank
import           Timely.Underwriting.Types (DenialReason)
import           Timely.Auth (Phone(..))
import           Timely.Types.Guid (Guid)
import           Timely.Types.Private
import           Timely.Types.Money as Money


instance Typeable t => SqlType (Token t) where
    mkLit (Token t) =  LCustom $ mkLit t
    sqlType _ = sqlType (Proxy :: Proxy Text)
    fromSql v = Token $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Text)

instance Typeable t => SqlType (Id t) where
    mkLit (Id t) =  LCustom $ mkLit t
    sqlType _ = sqlType (Proxy :: Proxy Text)
    fromSql v = Id $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Text)

instance SqlType Phone where
    mkLit (Phone t) =  LCustom $ mkLit t
    sqlType _ = sqlType (Proxy :: Proxy Text)
    fromSql v = Phone $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Text)


-- aggregate all account information
data Account = Account
    { accountId :: Guid Account
    , phone     :: Phone
    , customer  :: Customer
    , bankToken :: Private (Token Access)
    , credit    :: Money
    } deriving (Show, Eq, Generic)



data AccountRow = AccountRow
    { accountId :: Guid Account
    , phone     :: Phone
    , bankToken :: Private (Token Access)
    , credit    :: Money
    } deriving (Generic, Eq, Show)

instance SqlRow AccountRow


data Customer = Customer
    { id :: ID Customer
    , accountId :: Guid Account
    , firstName :: Text
    , middleName :: Maybe Text
    , lastName :: Text
    , email :: Text
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
    { id :: ID BankAccount
    , accountId :: Guid Account
    , accountType :: BankAccountType
    , bankAccountId :: Id Bank.Account
    , name :: Text
    , balance :: Money
    } deriving (Generic, Eq, Show)


instance SqlRow BankAccount




-- Can we have more than one application per phone number? NO
data Application = Application
    { accountId :: Guid Account
    , phone :: Phone
    , email :: Text
    , publicBankToken :: Token Public
    } deriving (Generic, Show)

instance SqlRow Application


data AppApproval = AppApproval
    { accountId :: Guid Account
    , approvalAmount :: Money
    } deriving (Generic, Show)

instance SqlRow AppApproval


data AppDenial = AppDenial
    { accountId :: Guid Account
    , denial :: DenialReason
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

