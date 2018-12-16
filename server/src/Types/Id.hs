module Types.Id where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (replicateM)
import Data.String.Conversions (cs)
import Data.Text (Text)
import System.Random (randomIO)

type Id s = Text


randomId :: MonadIO m => m (Id s)
randomId = do
    s <- liftIO $ replicateM 10 randomIO
    return $ cs (s :: String)

