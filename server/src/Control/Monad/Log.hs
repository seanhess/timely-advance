{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Log where


import Control.Monad.Logger (LoggingT, runStdoutLoggingT, logInfoN, logDebugN, logErrorN)
import Control.Monad.State (StateT, get, modify, evalStateT)
import Control.Monad.IO.Class (MonadIO)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text


-- TODO other mtl instances

class MonadLog m where
  info    :: Text -> m ()
  debug   :: Show a => a -> m ()
  error   :: Show a => a -> m ()
  context :: Text -> m ()


type LogT m = StateT [Text] (LoggingT m)

-- writer isn't quite right
instance MonadIO m => MonadLog (LogT m) where
  context t = modify $ \c -> c <> [t]

  info t = do
    c <- get
    logInfoN $ logMessage t c

  debug t = do
    c <- get
    logDebugN $ logMessage (cs $ show t) c

  error t = do
    c <- get
    logErrorN $ logMessage (cs $ show t) c


logMessage :: Text -> [Text] -> Text
logMessage t ctx = Text.unwords $ ctx <> [t]


runLogT :: MonadIO m => LogT m a -> m a
runLogT x = do
  let state = [] :: [Text]
  runStdoutLoggingT (evalStateT x state)
