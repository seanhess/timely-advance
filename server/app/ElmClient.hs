{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ElmClient where

import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource, toElmEncoderSource, toElmTypeSource, ElmType(..))
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (Proxy (Proxy), defElmImports, generateElmForAPI)

import Data.UUID (UUID)
import qualified Data.UUID as UUID


import Api (Api)
import Types.Account (Account)
import Types.Application (Application)
import Types.Account.Bank (Bank)
import Types.Account.Customer (Customer)
import Types.Account.AccountInfo (AccountInfo)



instance ElmType Account
instance ElmType Application
instance ElmType Bank
instance ElmType Customer
instance ElmType AccountInfo
instance ElmType UUID where
    toElmType u = toElmType $ UUID.toText u


spec :: Spec
spec = Spec ["Nimble", "Api"]
            [ defElmImports
             , toElmTypeSource    (Proxy :: Proxy Account)
             , toElmTypeSource    (Proxy :: Proxy AccountInfo)
             , toElmTypeSource    (Proxy :: Proxy Application)
             , toElmTypeSource    (Proxy :: Proxy Bank)
             , toElmTypeSource    (Proxy :: Proxy Customer)
             , toElmEncoderSource (Proxy :: Proxy AccountInfo)
             , toElmDecoderSource (Proxy :: Proxy AccountInfo)
             , toElmDecoderSource (Proxy :: Proxy Account)
             , toElmDecoderSource (Proxy :: Proxy Bank)
             , toElmDecoderSource (Proxy :: Proxy Customer)
             , toElmDecoderSource (Proxy :: Proxy Application)
             -- : generateElmForAPI  (Proxy :: Proxy Api))
            ]

main :: IO ()
main = do
    specsToDir [spec] "client"
