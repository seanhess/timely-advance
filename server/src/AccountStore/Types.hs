{-# LANGUAGE DuplicateRecordFields, DeriveGeneric #-}
module AccountStore.Types where



import Database.Selda
import Database.Selda.SqlType (Lit(..))
import Data.Aeson (ToJSON(..), FromJSON, Value(Null))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Types.Guid (Guid)
import Types.Plaid
import Types.Private
import Network.Plaid.Types (Token(..), Access)


-- there's no instance for ID!
instance ToJSON (ID a) where
    toJSON i = toJSON $ fromId i



-- an aggregate of the information
data Account = Account
    { accountId :: Guid Account
    , customer :: Customer
    , bankToken :: Private (Token Access)
    } deriving (Show, Eq, Generic)

instance ToJSON Account


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
instance ToJSON Customer


data BankAccountType
    = Checking
    | Savings
    | Credit
    | Other
    deriving (Generic, Eq, Show, Bounded, Enum, Read, Typeable)

instance SqlType BankAccountType
instance ToJSON BankAccountType

newtype Balance = Balance Int
    deriving (Generic, Eq, Show, Typeable)

instance SqlType Balance where
    mkLit (Balance b) =  LCustom $ mkLit b
    sqlType _ = sqlType (Proxy :: Proxy Int)
    fromSql v = Balance $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Int)

instance ToJSON Balance

data BankAccount = BankAccount
    { id :: ID BankAccount
    , accountId :: Guid Account
    , accountType :: BankAccountType
    , name :: Text
    , balance :: Balance
    } deriving (Generic, Eq, Show)


instance SqlRow BankAccount
instance ToJSON BankAccount






data Application = Application
    { accountId :: Guid Account
    , firstName :: Text
    , lastName :: Text
    , email :: Text
    , publicBankToken :: Token Public
    } deriving (Generic, Show)

instance SqlRow Application
instance ToJSON Application


