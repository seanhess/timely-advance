{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
module Timely.Notify where

import Prelude hiding (id)
import           Control.Monad.Config      (MonadConfig (..))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Service     (Service (..))
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           GHC.Generics              (Generic)
import           Servant.Client            (BaseUrl)
import qualified Servant.Client            as Servant
import           Timely.AccountStore.Types (Account)
import qualified Timely.AccountStore.Types as Account
import           Timely.Auth               (Phone (..))
import           Timely.Types.Guid         (Guid)
import Timely.Types.Guid as Guid
import qualified Twilio                    as Twilio
import qualified Twilio.Messages           as Twilio


data Notify x where
  Send :: Account -> Message a -> Notify ()



instance (MonadIO m, MonadConfig Config m) => Service m Notify where
  run (Send a m) = send a m



send
  :: (MonadIO m, MonadConfig Config m)
  => Account -> Message a -> m ()
send account message = do
  Config { fromPhone, accountSid, authToken, appBaseUrl } <- config
  let (Phone from) = "+1" <> fromPhone
      (Phone to)   = "+1" <> Account.phone (account :: Account)
  liftIO $ Twilio.runTwilio (accountSid, authToken) $ do
    Twilio.post $ Twilio.PostMessage to from (body appBaseUrl message) Nothing
  pure ()



body :: BaseUrl -> Message a -> Text
body b m = message m <> " " <> url b m



url :: BaseUrl -> Message a -> Text
url b m =
  cs (Servant.showBaseUrl b) <> "/#/" <> Text.intercalate "/" ["notify", rurl (resource m), Guid.toText (id m)]
  where rurl Advance = "advance"


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
  { fromPhone  :: Phone
  , accountSid :: Twilio.AccountSID
  , authToken  :: Twilio.AuthToken
  , appBaseUrl :: BaseUrl
  }
