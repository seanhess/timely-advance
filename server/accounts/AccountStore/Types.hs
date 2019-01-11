{-# LANGUAGE DuplicateRecordFields, DeriveGeneric #-}
module AccountStore.Types where



import Database.Selda
import Database.Selda.SqlType (Lit(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import           Bank (Token(..), Public, Access, Id(..))
import qualified Bank
import Underwriting.Types (DenialReason)
import Types.Guid (Guid)
import Types.Private
import Types.Money


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


-- an aggregate of the information
data Account = Account
    { accountId :: Guid Account
    , customer :: Customer
    , bankToken :: Private (Token Access)
    } deriving (Show, Eq, Generic)



data AccountRow = AccountRow
    { accountId :: Guid Account
    , bankToken :: Private (Token Access)
    } deriving (Generic, Eq, Show)

instance SqlRow AccountRow


data Customer = Customer
    { id :: ID Customer
    , accountId :: Guid Account
    , firstName :: Text
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




data Application = Application
    { accountId :: Guid Account
    , firstName :: Text
    , lastName :: Text
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


