module Main where

import System.IO (hSetBuffering, stdout, stderr, BufferMode(..))

import qualified Api
import SeldaTutorial


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    Api.run 3001



