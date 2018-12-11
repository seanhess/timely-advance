module AppM where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Data.Text (Text)
import Data.Account (Account)
import Servant (Handler)


data AppState = AppState
    { appMessage :: Text
    , appAccounts :: TVar [ Account ]
    }


newState :: Text -> IO AppState
newState m = do
    as <- atomically $ newTVar []
    return $ AppState m as




type AppM = ReaderT AppState Handler


nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT x s

