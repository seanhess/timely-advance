module Main where

import qualified Api

main :: IO ()
main = do
    Api.run 3001

