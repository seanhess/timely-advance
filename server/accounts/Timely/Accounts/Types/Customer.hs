{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Timely.Accounts.Types.Customer where


import           Data.Aeson                    (ToJSON (..))
import           Data.Model.Guid               (Guid)
import           Data.Model.Types              (PostalCode, SSN, State)
import           Data.Model.Valid              (Valid (..))
import           Data.Text                     as Text
import           Data.Time.Calendar            (Day)
import           Data.Time.Clock               (UTCTime)
import           Database.Selda                (ID, SqlRow (..))
import           GHC.Generics                  (Generic)
import           Timely.Accounts.Types.Account (Account)
import           Timely.Accounts.Types.Api     ()




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
instance ToJSON Customer
