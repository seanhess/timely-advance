{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
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
  , authenticate
  , transfer
  , searchCustomers

  , fundingSource
  , Credentials(..)
  , Customer(..)
  , Resource(..)
  , Static(..)
  , Id(..)
  , CreateFundingSource(..)
  , FundingSource
  , Client, Secret
  , AuthToken(..)
  , Amount(..)
  , DwollaError(..)
  , Config(..)
  , Transfer
  , SearchOptions(..)
  ) where




import           Control.Monad.Catch       (MonadThrow, MonadCatch, throwM, catch)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Aeson                as Aeson
import qualified Data.ByteString.Base64    as Base64
import           Data.Model.Id             (Id (..), Token (..))
import           Data.Proxy                as Proxy
import           Data.String.Conversions   (cs)
import           Data.Text                 as Text
import           GHC.Generics              (Generic)
import           Network.Dwolla.Errors     (DwollaError (..), dwollaError)
import           Network.Dwolla.HAL        (FromHAL (..), HAL)
import           Network.Dwolla.Types
import qualified Network.HTTP.Client       as HTTP
import           Network.Plaid.Dwolla      (Dwolla)
import           Servant
import           Servant.Client            (BaseUrl, ClientM, client)
import qualified Servant.Client            as Servant
import           Web.FormUrlEncoded        (ToForm (..))



-- Main Actions ----------------------------


authenticate :: (MonadIO m, MonadThrow m) => Config -> m AuthToken
authenticate config = runDwollaAuth config $ reqAuthenticate (credentials config)


createCustomer :: (MonadIO m, MonadThrow m, MonadCatch m) => Config -> AuthToken -> Customer -> m (Id Customer)
createCustomer config tok cust =
  catch (runDwolla config $ reqCreateCustomer tok cust) onDuplicate



createFundingSource :: (MonadIO m, MonadCatch m, MonadThrow m) => Config -> AuthToken -> Id Customer -> CreateFundingSource -> m (Id FundingSource)
createFundingSource config tok id cfs =
  catch (runDwolla config $ reqCreateFundingSource tok id cfs) onDuplicate


transfer :: (MonadIO m, MonadThrow m) => Config -> AuthToken -> Id FundingSource -> Id FundingSource -> Amount -> m (Id Transfer)
transfer config tok from to amount = do
  let base = baseUrl config
  runDwolla config $ reqTransfer tok (fundingSource base from) (fundingSource base to) amount


searchCustomers :: (MonadIO m, MonadThrow m) => Config -> AuthToken -> Text -> Maybe SearchOptions -> m [CustomerResult]
searchCustomers config tok search opts =
  runDwolla config $ reqCustomers tok search opts



onDuplicate :: (MonadThrow m, MonadIO m) => DwollaError -> m (Id a)
onDuplicate (Duplicate (Id i)) = pure (Id i)
onDuplicate err = throwM err



-- Remote API --------------------------

type DwollaApi
      = Authenticate :> "token" :> ReqBody '[FormUrlEncoded] Auth :> Post '[JSON] Access
   :<|> Authorization   :> "customers" :> ReqBody '[HAL] Customer   :> Post '[HAL] (Location Customer)
   :<|> Authorization   :> "customers" :> QueryParam "search" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[HAL] (Embedded Customers)
   :<|> Authorization   :> "customers" :> Capture "id" (Id Customer) :> "funding-sources" :> ReqBody '[HAL] CreateFundingSource :> Post '[HAL] (Location CreateFundingSource)
   :<|> Authorization   :> "transfers" :> ReqBody '[HAL] Transfer   :> Post '[HAL] (Location Transfer)


type Authorization = Header "Authorization" AuthToken


type FundingSourceApi
   = "funding-sources" :> Capture "id" (Id FundingSource) :> Get '[JSON] ()


type Location a = Headers '[Header "Location" (Resource a)] Anything

type Authenticate = Header "Authorization" Credentials


postToken         :: Maybe Credentials -> Auth -> ClientM Access
postCustomer      :: Maybe AuthToken -> Customer -> ClientM (Location Customer)
getCustomers      :: Maybe AuthToken -> Maybe Text -> Maybe Int -> Maybe Int -> ClientM (Embedded Customers)
postFundingSource :: Maybe AuthToken -> Id Customer -> FundingSource -> ClientM (Location FundingSource)
postTransfer      :: Maybe AuthToken -> Transfer -> ClientM (Location Transfer)
postToken :<|> postCustomer :<|> getCustomers :<|> postFundingSource :<|> postTransfer = client (Proxy :: Proxy DwollaApi)




data SearchOptions = SearchOptions
  { limit  :: Int
  , offset :: Int
  }


reqCustomers :: AuthToken -> Text -> Maybe SearchOptions -> ClientM [CustomerResult]
reqCustomers tok search mopts = do
  emb <- getCustomers (Just tok) (Just search) (limit <$> mopts) (offset <$> mopts)
  pure $ customers $ _embedded emb



reqCreateCustomer :: AuthToken -> Customer -> ClientM (Id Customer)
reqCreateCustomer tok cust =
  postCustomer (Just tok) cust >>= parseId


reqCreateFundingSource :: AuthToken -> Id Customer -> FundingSource -> ClientM (Id FundingSource)
reqCreateFundingSource tok id source =
  postFundingSource (Just tok) id source >>= parseId



reqTransfer :: AuthToken -> Resource FundingSource -> Resource FundingSource -> Amount -> ClientM (Id Transfer)
reqTransfer tok from to amount = do
  let tamount = TransferAmount Static amount
      links   = TransferLinks (RelLink from) (RelLink to)
  res <- postTransfer (Just tok) $ Transfer links tamount
  parseId res



reqAuthenticate :: Credentials -> ClientM AuthToken
reqAuthenticate creds = do
  access <- postToken (Just creds) (Auth Static)
  let (Token t) = access_token access
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
locationToId (Headers _ (HCons (Header res) HNil)) =
  resourceToId res
locationToId _ = Nothing













data Customer = Customer
  { firstName :: Text
  , lastName  :: Text
  , email     :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Customer

data CreateFundingSource = CreateFundingSource
  { plaidToken :: Token Dwolla
  , name       :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON CreateFundingSource
type FundingSource = CreateFundingSource


data Transfer = Transfer
  { _links :: TransferLinks
  , amount :: TransferAmount
  } deriving (Show, Eq, Generic)

instance ToJSON Transfer

data TransferLinks = TransferLinks
  { source      :: RelLink CreateFundingSource
  , destination :: RelLink CreateFundingSource
  } deriving (Show, Eq, Generic)

instance ToJSON TransferLinks

data TransferAmount = TransferAmount
  { currency :: Static "USD"
  , value    :: Amount
  } deriving (Show, Eq, Generic)

instance ToJSON TransferAmount



-- _embedded.customers -->
data Embedded a = Embedded
  { _embedded :: a
  } deriving (Generic)

instance FromJSON a => FromJSON (Embedded a)
instance (FromJSON a) => FromHAL (Embedded a)

data Customers = Customers
  { customers :: [CustomerResult]
  } deriving (Generic)

instance FromJSON Customers

-- https://docs.dwolla.com/#list-and-search-customers
data CustomerResult = CustomerResult
  { id        :: Id Customer
  , firstName :: Text
  , lastName  :: Text
  , email     :: Text
  -- type, status, created
  } deriving (Generic)

instance FromJSON CustomerResult




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





-- Dwolla API Helpers ----------------------------------------------

data Config = Config
    { manager     :: HTTP.Manager
    , baseUrl     :: BaseUrl
    , baseUrlAuth :: BaseUrl
    , credentials :: Credentials
    }



-- make it so you can only run authenticate with the right one

runDwollaAuth :: (MonadThrow m, MonadIO m) => Config -> ClientM a -> m a
runDwollaAuth config req =
    runDwollaClient baseUrlAuth config req


runDwolla :: (MonadThrow m, MonadIO m) => Config -> ClientM a -> m a
runDwolla config req =
    runDwollaClient baseUrl config req


runDwollaClient :: (MonadThrow m, MonadIO m) => (Config -> BaseUrl) -> Config -> ClientM a -> m a
runDwollaClient url config req = do
    let env = Servant.mkClientEnv (manager config) (url config)
    res <- liftIO $ Servant.runClientM req env
    case res of
      Left err -> throwM $ dwollaError err
      Right a  -> pure a
