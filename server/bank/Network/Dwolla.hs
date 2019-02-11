{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
module Network.Dwolla
  ( DwollaApi
  , createCustomer
  , createFundingSource
  , fundingSource
  , transfer
  , authenticate
  , Customer(..)
  , Resource(..)
  , Static(..)
  , Address(..)
  , Last4SSN(..)
  , PhoneDigits(..)
  , Id(..)
  , FundingSource(..)
  ) where



import           Control.Monad.Catch      (Exception, throwM)
import           Data.Aeson               as Aeson
import           Data.List                as List
import           Data.List.NonEmpty       (NonEmpty ((:|)))
import           Data.Proxy               as Proxy
import           Data.String.Conversions  (cs)
import           Data.Text                as Text
import           Data.Time.Calendar       (Day)
import           GHC.Generics             (Generic)
import           GHC.TypeLits             (KnownSymbol, Symbol, symbolVal)
import           Network.HTTP.Media       ((//))
import           Network.Plaid.Dwolla     (Dwolla)
import qualified Network.Plaid.Types      as Plaid
import qualified Numeric
import           Servant
import qualified Servant.API.ContentTypes as Servant
import           Servant.Client           (BaseUrl, ClientM, client)
import qualified Servant.Client           as Servant
import           Web.FormUrlEncoded       (ToForm (..))





type DwollaApi
      = BasicAuth "" () :> "token" :> ReqBody '[FormUrlEncoded] Auth :> Post '[HAL] Access
   :<|> Authorization   :> "customers" :> ReqBody '[HAL] Customer   :> Post '[HAL] (Location Customer)
   :<|> Authorization   :> "customers" :> Capture "id" (Id Customer) :> ReqBody '[HAL] FundingSource :> Post '[HAL] (Location FundingSource)
   :<|> Authorization   :> "transfers" :> ReqBody '[HAL] Transfer   :> Post '[HAL] (Location Transfer)


type Authorization = Header "Authorization" (Bearer Access)


type FundingSourceApi
   = "funding-sources" :> Capture "id" (Id FundingSource) :> Get '[JSON] ()


type Location a = Headers '[Header "Location" (Resource a)] NoContent


postToken         :: BasicAuthData -> Auth -> ClientM Access
postCustomer      :: Maybe (Bearer Access) -> Customer -> ClientM (Location Customer)
postFundingSource :: Maybe (Bearer Access) -> Id Customer -> FundingSource -> ClientM (Location FundingSource)
postTransfer      :: Maybe (Bearer Access) -> Transfer -> ClientM (Location Transfer)
postToken :<|> postCustomer :<|> postFundingSource :<|> postTransfer = client (Proxy :: Proxy DwollaApi)



createCustomer :: Token Access -> Customer -> ClientM (Id Customer)
createCustomer tok cust =
  postCustomer (auth tok) cust >>= parseId


createFundingSource :: Token Access -> Id Customer -> FundingSource -> ClientM (Id FundingSource)
createFundingSource tok id source =
  postFundingSource (auth tok) id source >>= parseId



transfer :: Token Access -> Resource FundingSource -> Resource FundingSource -> Amount -> ClientM (Id Transfer)
transfer tok from to amount = do
  let tamount = TransferAmount Static amount
      links   = TransferLinks (RelLink from) (RelLink to)
  res <- postTransfer (auth tok) $ Transfer links tamount
  parseId res



authenticate :: Id Client -> Id Secret -> ClientM (Token Access)
authenticate (Id u) (Id p) = do
  access <- postToken (BasicAuthData (cs u) (cs p)) (Auth Static)
  pure $ access_token access


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


-- HAL ----------------------------------------

data HAL


instance Accept HAL where
  contentTypes _ = "application" // "vnd.dwolla.v1.hal+json" :| []

instance FromJSON a => MimeUnrender HAL a where
  mimeUnrender _ b = Servant.eitherDecodeLenient b

instance ToJSON a => MimeRender HAL a where
  mimeRender _ = encode





-- Types ----------------------------------------


newtype Id a = Id Text
  deriving (Show, Eq, Generic, ToHttpApiData, FromJSON)

type Token = Id

newtype Resource a = Resource Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON, FromHttpApiData)

data Static (s :: Symbol) = Static
  deriving (Show, Eq)

instance forall s. KnownSymbol s => ToJSON (Static s) where
  toJSON _ = Aeson.String $ cs $ symbolVal (Proxy :: Proxy s)

instance forall s. KnownSymbol s => ToHttpApiData (Static s) where
  toUrlPiece _ = cs $ symbolVal (Proxy :: Proxy s)

newtype Last4SSN = Last4SSN Text
  deriving (Show, Eq, Generic, ToJSON)

newtype PhoneDigits = PhoneDigits Text
  deriving (Show, Eq, Generic, ToJSON)

newtype Address = Address Text
  deriving (Show, Eq, Generic)

instance ToJSON Address where
  toJSON (Address t) = Aeson.String $ Text.take 50 t

newtype Amount = Amount Float
  deriving (Show, Eq)

instance ToJSON Amount where
  toJSON (Amount f) = Aeson.String $ cs $ Numeric.showFFloat (Just 2) f ""

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
  , postalCost  :: Text
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

newtype Bearer a = Bearer Text

instance ToHttpApiData (Bearer a) where
  toUrlPiece (Bearer t) = "Bearer: " <> t

auth :: Token Access -> Maybe (Bearer Access)
auth (Id a) = Just $ Bearer a
