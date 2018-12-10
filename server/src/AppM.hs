module AppM where

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Text (Text)
import Data.Account (Account)
import Servant (Handler)


type Config = Text


data AppState = AppState
    { accounts :: [ Account ] }


type AppM = ReaderT Config Handler


nt :: Config -> AppM a -> Handler a
nt s x = runReaderT x s

