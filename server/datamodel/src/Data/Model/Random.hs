module Data.Model.Random where


import           Control.Monad          (replicateM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List              as List
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified System.Random          as Random




chars :: String
chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']


randomC :: MonadIO m => m Char
randomC = liftIO $ do
  n <- Random.randomRIO (0, 61)
  pure $ chars !! n


randomAZ :: MonadIO m => Int -> m Text
randomAZ n = liftIO $ do
  cs <- replicateM n randomC
  pure $ Text.pack cs
