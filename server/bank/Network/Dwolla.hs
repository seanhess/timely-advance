{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Network.Dwolla
  ( DwollaApi
  , createCustomer
  , createFundingSource
  , fundingSource
  , transfer
  , authenticate
  , Credentials(..)
  , Customer(..)
  , Resource(..)
  , Static(..)
  , Address(..)
  , Last4SSN(..)
  , PhoneDigits(..)
  , Id(..)
  , FundingSource(..)
  , Client, Secret
  , AuthToken(..)
  ) where




import           Control.Monad.Catch     (Exception, throwM)
import           Data.Aeson              as Aeson
import           Data.List               as List
import           Data.Proxy              as Proxy
import qualified Data.ByteString.Base64 as Base64
import           Data.String.Conversions (cs)
import           Data.Text               as Text
import           Data.Time.Calendar      (Day)
import           GHC.Generics            (Generic)
import           Network.Plaid.Dwolla    (Dwolla)
import qualified Network.Plaid.Types     as Plaid
import           Servant
import           Servant.Client          (BaseUrl, ClientM, client)
import qualified Servant.Client          as Servant
import           Web.FormUrlEncoded      (ToForm (..))

import           Network.Dwolla.HAL      (HAL, FromHAL(..))
import           Network.Dwolla.Types





type DwollaApi
      = Authenticate :> "token" :> ReqBody '[FormUrlEncoded] Auth :> Post '[JSON] Access
   :<|> Authorization   :> "customers" :> ReqBody '[HAL] Customer   :> Post '[HAL] (Location Customer)
   :<|> Authorization   :> "customers" :> Capture "id" (Id Customer) :> ReqBody '[HAL] FundingSource :> Post '[HAL] (Location FundingSource)
   :<|> Authorization   :> "transfers" :> ReqBody '[HAL] Transfer   :> Post '[HAL] (Location Transfer)


type Authorization = Header "Authorization" AuthToken


type FundingSourceApi
   = "funding-sources" :> Capture "id" (Id FundingSource) :> Get '[JSON] ()


type Location a = Headers '[Header "Location" (Resource a)] Anything

type Authenticate = Header "Authorization" Credentials


postToken         :: Maybe Credentials -> Auth -> ClientM Access
postCustomer      :: Maybe AuthToken -> Customer -> ClientM (Location Customer)
postFundingSource :: Maybe AuthToken -> Id Customer -> FundingSource -> ClientM (Location FundingSource)
postTransfer      :: Maybe AuthToken -> Transfer -> ClientM (Location Transfer)
postToken :<|> postCustomer :<|> postFundingSource :<|> postTransfer = client (Proxy :: Proxy DwollaApi)



createCustomer :: AuthToken -> Customer -> ClientM (Id Customer)
createCustomer tok cust =
  postCustomer (Just tok) cust >>= parseId


createFundingSource :: AuthToken -> Id Customer -> FundingSource -> ClientM (Id FundingSource)
createFundingSource tok id source =
  postFundingSource (Just tok) id source >>= parseId



transfer :: AuthToken -> Resource FundingSource -> Resource FundingSource -> Amount -> ClientM (Id Transfer)
transfer tok from to amount = do
  let tamount = TransferAmount Static amount
      links   = TransferLinks (RelLink from) (RelLink to)
  res <- postTransfer (Just tok) $ Transfer links tamount
  parseId res



authenticate :: Credentials -> ClientM AuthToken
authenticate creds = do
  access <- postToken (Just creds) (Auth Static)
  let (Id t) = access_token access
  pure $ AuthToken t


fundingSource :: BaseUrl -> Id FundingSource -> Resource FundingSource
fundingSource base id = Resource $ cs $ Servant.showBaseUrl base <> "/" <> cs (Servant.toUrlPiece (link id))
  where
    link = Servant.safeLink proxy proxy
    proxy = Proxy :: Proxy FundingSourceApi




parseId :: Location a -> ClientM (Id a)
parseId loc =
  case locationToId loc of
    Nothing -> throwM BadLocation
    Just id -> pure id



locationToId :: Location a -> Maybe (Id a)
locationToId (Headers _ (HCons (Header (Resource a)) HNil)) =
  Just $ Id $ Text.takeWhileEnd ((/=) '/') a
locationToId _ = Nothing




data DwollaError
  = BadLocation
  deriving (Show, Eq, Generic)

instance Exception DwollaError








data Customer = Customer
  { firstName   :: Text
  , lastName    :: Text
  , email       :: Text
  , ipAddress   :: Maybe Text
  , type_       :: Static "personal"
  , address1    :: Address
  , address2    :: Maybe Address
  , city        :: Text
  , state       :: Text
  , postalCode  :: Text
  , dateOfBirth :: Day
  , ssn         :: Last4SSN
  , phone       :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON Customer where
  toJSON = Aeson.genericToJSON removeUnderscores

removeUnderscores :: Aeson.Options
removeUnderscores = Aeson.defaultOptions { fieldLabelModifier = List.dropWhileEnd ((==) '_') }


data FundingSource = FundingSource
  { plaidToken :: Plaid.Token Dwolla
  , name       :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON FundingSource


data Transfer = Transfer
  { _links :: TransferLinks
  , amount :: TransferAmount
  } deriving (Show, Eq, Generic)

instance ToJSON Transfer

data TransferLinks = TransferLinks
  { source      :: RelLink FundingSource
  , destination :: RelLink FundingSource
  } deriving (Show, Eq, Generic)

instance ToJSON TransferLinks

data TransferAmount = TransferAmount
  { currency :: Static "USD"
  , value    :: Amount
  } deriving (Show, Eq, Generic)

instance ToJSON TransferAmount

data RelLink a = RelLink
  { href :: Resource a
  } deriving (Show, Eq, Generic)


instance ToJSON (RelLink a)




data Access = Access
  { access_token :: Token Access
  , token_type   :: Text
  , expires_in   :: Int
  } deriving (Show, Generic, Eq)

instance FromJSON Access


data Auth = Auth
  { grant_type :: Static "client_credentials"
  } deriving (Show, Generic, Eq)

instance ToForm Auth



data Secret
data Client


newtype AuthToken = AuthToken Text
   deriving (Show, Eq)

instance ToHttpApiData AuthToken where
  toUrlPiece (AuthToken t) = "Bearer " <> t



data Credentials = Credentials (Id Client) (Id Secret)
   deriving (Show, Eq)

instance ToHttpApiData Credentials where
  toUrlPiece (Credentials (Id c) (Id s)) = cs $ "Basic " <> Base64.encode (cs c <> ":" <> cs s)


data Anything = Anything

instance FromHAL Anything where
    fromHAL _ = pure Anything
