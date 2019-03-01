{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Timely.Notify where

import           Control.Monad.Config      (MonadConfig (..))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Service     (Service (..))
import           Data.Model.Guid           as Guid
import           Data.Model.Types          (Phone, Valid (..))
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Prelude                   hiding (id)
import           Servant.Client            (BaseUrl)
import qualified Servant.Client            as Servant
import           Timely.AccountStore.Types (Account (..))
import qualified Timely.AccountStore.Types as Account
import qualified Twilio                    as Twilio
import qualified Twilio.Messages           as Twilio


data Notify x where
  Send :: Typeable a => Account -> Message a -> Notify ()



instance (MonadIO m, MonadConfig Config m) => Service m Notify where
  run (Send a m) = send a m



send
  :: (MonadIO m, MonadConfig Config m, Typeable a)
  => Account -> Message a -> m ()
send account message = do
  Config { fromPhone, accountSid, authToken, appBaseUrl } <- config
  let (Valid from) = fromPhone
      (Valid to)   = Account.phone (account :: Account)
  liftIO $ Twilio.runTwilio (accountSid, authToken) $ do
    Twilio.post $ Twilio.PostMessage ("+1" <> to) ("+1" <> from) (body appBaseUrl (accountId account) message) Nothing
  pure ()



body :: Typeable a => BaseUrl -> Guid Account -> Message a -> Text
body b a m = message m <> " " <> url b a m



url :: forall a. Typeable a => BaseUrl -> Guid Account -> Message a -> Text
url b a m =
  cs (Servant.showBaseUrl b) <> "/#/" <> Text.intercalate "/"
    [ "accounts"
    , Guid.toText a
    , rurl (resource m)
    , Guid.toText (id m)
    ]
  where rurl Advance = "advances"


data Resource
  = Advance
  deriving (Show, Eq, Generic)


data Message a = Message
  { id       :: Guid a
  , resource :: Resource
  , message  :: Text
  } deriving (Show, Eq, Generic)


-- phone number in 10-digit format
data Config = Config
  { fromPhone  :: Valid Phone
  , accountSid :: Twilio.AccountSID
  , authToken  :: Twilio.AuthToken
  , appBaseUrl :: BaseUrl
  }
