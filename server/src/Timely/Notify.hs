{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
module Timely.Notify where

import           Control.Effects           (Effect (..), MonadEffect (..), RuntimeImplemented, effect, implement)
import           Control.Monad.Config      (MonadConfig (..))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans       (lift)
import           Data.Model.Guid           as Guid
import           Data.Model.Types          (Phone, Valid (..))
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           GHC.Generics              (Generic)
import           Prelude                   hiding (id)
import           Servant.Client            (BaseUrl)
import qualified Servant.Client            as Servant
import           Timely.AccountStore.Types (Account, AccountRow (..))
import qualified Timely.AccountStore.Types as Account
import qualified Twilio                    as Twilio
import qualified Twilio.Messages           as Twilio


data Notify m = NotifyMethods
  { _send :: forall a. GuidPrefix a => AccountRow -> Message a -> m ()
  }

instance Effect Notify where
  liftThrough methods = NotifyMethods
    (\a m -> lift (_send methods a m))

  mergeContext mm = NotifyMethods
    (\i t -> do
        m <- mm
        _send m i t)


send :: (MonadEffect Notify m, GuidPrefix a) => AccountRow -> Message a -> m ()
send = _send effect


implementIO :: (MonadIO m, MonadConfig Config m) => RuntimeImplemented Notify m a -> m a
implementIO = implement $
  NotifyMethods
    sendMessage


-- instance (MonadIO m, MonadConfig Config m) => Service m Notify where
--   run (Send a m) = send a m



sendMessage
  :: (MonadIO m, MonadConfig Config m, GuidPrefix a)
  => AccountRow -> Message a -> m ()
sendMessage account message = do
  Config { fromPhone, accountSid, authToken, appBaseUrl } <- config
  let (Valid from) = fromPhone
      (Valid to)   = Account.phone (account :: AccountRow)
  liftIO $ Twilio.runTwilio (accountSid, authToken) $ do
    Twilio.post $ Twilio.PostMessage ("+1" <> to) ("+1" <> from) (body appBaseUrl (accountId account) message) Nothing
  pure ()



body :: GuidPrefix a => BaseUrl -> Guid Account -> Message a -> Text
body b a m = message m <> " " <> url b a m



url :: forall a. GuidPrefix a => BaseUrl -> Guid Account -> Message a -> Text
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
