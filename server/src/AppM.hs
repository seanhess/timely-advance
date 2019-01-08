{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AppM
  ( AppState(..)
  , loadState
  , nt
  , AppM
  , clientConfig
  ) where

import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.String.Conversions (cs)
import Database.Selda (MonadMask(..))
import Database.Selda.Backend (SeldaConnection, MonadSelda(..), SeldaT, runSeldaT)
import qualified Database.Selda.PostgreSQL as Selda
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Network.AMQP.Worker as Worker
import Network.AMQP.Worker.Monad (MonadWorker(..))
import qualified Network.Plaid.Types as Plaid
import Network.Plaid.Types (Id(..), Client, Secret, Public)
import Servant (Handler(..), runHandler)
import System.Envy (FromEnv, DefConfig(..), Var(..))
import qualified System.Envy as Envy
import Types.Config (ClientConfig(ClientConfig), PlaidConfig(PlaidConfig))


data AppState = AppState
    { env :: Env
    , dbConn :: SeldaConnection
    , amqpConn :: Worker.Connection
    , plaid :: Plaid.Credentials
    }


data Env = Env
    { postgres :: Text
    , amqp :: Text
    , plaidPublicKey :: Id Public
    , plaidClientId :: Id Client
    , plaidClientSecret :: Id Secret
    } deriving (Show, Eq, Generic)


instance DefConfig Env where
    defConfig = Env
      { postgres = "postgresql://postgres@localhost:5432"
      , amqp = "amqp://guest:guest@localhost:5672"
      , plaidPublicKey = Id "447ab26f3980c45b7202e2006dd9bf"
      , plaidClientId = Id "5c1a663c5eca930011ff67ee"
      , plaidClientSecret = Id "db8bad5d68d41340cba767615c7aea"
      }

instance FromEnv Env
instance Typeable t => Var (Id t) where
    toVar (Id t) = toVar t
    fromVar s = Id <$> fromVar s


loadState :: (MonadIO m, MonadMask m) => m AppState
loadState = do
    eenv <- liftIO $ Envy.decodeEnv
    case eenv of
      Left err -> error err
      Right env -> do
        amqpConn <- Worker.connect (Worker.fromURI $ cs $ amqp env)
        dbConn <- Selda.pgOpen' Nothing (cs $ postgres env)
        let plaid = Plaid.Credentials (plaidClientId env) (plaidClientSecret env)
        pure $ AppState {..}


clientConfig :: AppM ClientConfig
clientConfig = do
    e <- asks env
    pure $ ClientConfig $ PlaidConfig (plaidPublicKey e)


type AppM = ReaderT AppState Handler



instance MonadSelda AppM where
    seldaConnection = asks dbConn

instance MonadWorker AppM where
    amqpConnection = asks amqpConn

-- TODO upgrade to servant 0.15 and remove this
deriving instance MonadMask Handler



nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT x s


