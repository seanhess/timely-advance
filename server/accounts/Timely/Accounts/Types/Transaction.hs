{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Timely.Accounts.Types.Transaction where


import           Data.Aeson                    (ToJSON (..))
import           Data.Model.Guid               (Guid)
import           Data.Model.Id                 (Id (..))
import           Data.Model.Money              as Money
import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Time.Calendar            (Day)
import           Database.Selda                (SqlRow (..))
import           Database.Selda.SqlType        (Lit (..), SqlType (..))
import           GHC.Generics                  (Generic)
import           Timely.Accounts.Types.Account (Account)
import qualified Timely.Bank                   as Bank (Category (..), Currency (..), Transaction (..))




-- Transaction -----------------------------

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
instance ToJSON Transaction


newtype Category = Category Text
  deriving (Show, Eq, Generic, ToJSON)

instance SqlType Category where
    mkLit (Category b) =  LCustom $ mkLit b
    sqlType _ = sqlType (Proxy :: Proxy Text)
    fromSql v = Category $ fromSql v
    defaultValue = LCustom (defaultValue :: Lit Text)



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
   cats _         = ""
   cat (Bank.Category c) = c
