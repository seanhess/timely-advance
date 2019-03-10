module Timely.Bank.Types where

import           Control.Exception      (Exception)
import           Data.Model.Types       (Address (..))
import           Data.Text              (Text)
import           Network.HTTP.Client    (Manager)
import           Network.Plaid.Identity (AddressInfo (..))
import           Network.Plaid.Types    (Credentials)
import           Servant.Client         (BaseUrl, ServantError)

data Config = Config
    { manager     :: Manager
    , baseUrl     :: BaseUrl
    , credentials :: Credentials
    }




data BankError
    = BadName Text
    | NoNames
    | BadAddresses [AddressInfo]
    | PlaidError ServantError
    deriving (Eq, Show)

instance Exception BankError



data Identity = Identity
    { names   :: Names
    , address :: Address
    } deriving (Show, Eq)

data Names = Names
    { firstName  :: Text
    , middleName :: Maybe Text
    , lastName   :: Text
    } deriving (Show, Eq)
