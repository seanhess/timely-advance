{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AppM where

import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Database.Selda (MonadMask(..))
import Database.Selda.Backend (SeldaConnection, MonadSelda(..), SeldaT, runSeldaT)
import Data.Text (Text)
import Servant (Handler(..), runHandler)
import Types.Config


data AppState = AppState
    { appMessage :: Text
    , connection :: SeldaConnection
    , client :: ClientConfig
    }



type AppM = ReaderT AppState Handler



instance MonadSelda AppM where
    seldaConnection = asks connection

-- TODO upgrade to servant 0.15 and remove this
deriving instance MonadMask Handler



nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT x s


