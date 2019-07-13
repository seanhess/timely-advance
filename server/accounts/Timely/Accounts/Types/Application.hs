{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Timely.Accounts.Types.Application where


import Data.Aeson                    (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Maybe                    (fromMaybe)
import Data.Model.Guid               (Guid)
import Data.Model.Id                 (Id (..), Token)
import Data.Model.Types              (Phone)
import Data.Model.Valid              (Valid (..))
import Data.String.Conversions       (cs)
import Data.Text                     as Text
import Data.Time.Clock               (UTCTime)
import Database.Selda                (SqlRow (..))
import Database.Selda.SqlType        (Lit (..), SqlType (..), SqlTypeRep (..), SqlValue (..))
import GHC.Generics                  (Generic)
import Text.Read                     (readMaybe)
import Timely.Accounts.Types.Account (Account)
import Timely.Accounts.Types.Api     ()
import Timely.Bank                   (Item, Public)



-- Can we have more than one application per phone number? NO
data Application = Application
    { accountId       :: Guid Account
    , phone           :: Valid Phone
    , publicBankToken :: Token Public
    , created         :: UTCTime
    , onboarding      :: Onboarding
    , email           :: Text
    } deriving (Generic, Show)


instance SqlRow Application
instance ToJSON Application
instance FromJSON Application


data Onboarding
    = Pending Pending
    | Rejected Rejected
    | Error
    | Complete
    deriving (Show, Eq, Generic, Read)


data Pending
    = New
    | Bank
    | Transfers
    | Transactions
    | Creation
    deriving (Read, Show, Eq, Generic)
instance ToJSON Pending

data Rejected
    = IncomeLow
    | IncomeNotRegular
    deriving (Read, Show, Eq, Generic)
instance ToJSON Rejected

fromOnboarding :: Onboarding -> Text
fromOnboarding = cs . show

toOnboarding :: Text -> Maybe Onboarding
toOnboarding = readMaybe . cs


instance ToJSON Onboarding where
  toJSON = String . fromOnboarding

instance FromJSON Onboarding where
  parseJSON = withText "Onboarding" $ \t ->
    case toOnboarding t of
      Just o  -> pure o
      Nothing -> fail $ "expected onboarding, but encountered: " ++ cs t

instance SqlType Onboarding where
  mkLit = LCustom . LText . fromOnboarding
  sqlType _ = TText
  fromSql (SqlString t) =
    fromMaybe (error $ "fromSql: Onboarding column with invalid string: " ++ cs t) $ toOnboarding t
  fromSql s = error $ "fromSql: Onboarding column with non string: " ++ show s
  defaultValue = mkLit $ Pending New



-- save this as soon as we have the bank id
data AppBank = AppBank
    { accountId    :: Guid Account
    , bankItemId   :: Id Item
    , transactions :: Maybe Int
    } deriving (Generic, Show)

instance SqlRow AppBank
