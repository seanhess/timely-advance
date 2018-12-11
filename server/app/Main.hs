module Main where

import qualified Api
import qualified BeamTutorial

main :: IO ()
main = do
    Api.run 3001

