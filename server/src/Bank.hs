{-# LANGUAGE GADTs #-}
module Bank
    ( Token
    , Access
    , Currency
    ) where

import Network.Plaid.Types (Token, Access, Currency, Credentials)

-- Bank Service

data Bank a where
    LoadBalance :: Token Access -> Bank Currency



loadBalance :: Credentials -> Token Access -> m Currency
loadBalance = do
    undefined
