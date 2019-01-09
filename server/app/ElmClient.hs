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


import Database.Selda (ID, fromId)
import Api (Api)
import AccountStore.Types
import Types.Account (AccountInfo)
import Types.Private (Private)
import Network.Plaid.Types (Token)



instance ElmType a => ElmType (Private a)
instance ElmType (ID a) where
    toElmType i = toElmType $ fromId i
instance ElmType Account
instance ElmType Application
instance ElmType (Token t)
instance ElmType BankAccount
instance ElmType BankAccountType
instance ElmType Balance
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
             , toElmTypeSource    (Proxy :: Proxy BankAccount)
             , toElmTypeSource    (Proxy :: Proxy BankAccountType)
             , toElmTypeSource    (Proxy :: Proxy Customer)
             , toElmEncoderSource (Proxy :: Proxy AccountInfo)
             , toElmDecoderSource (Proxy :: Proxy AccountInfo)
             , toElmDecoderSource (Proxy :: Proxy Account)
             , toElmDecoderSource (Proxy :: Proxy BankAccount)
             -- , toElmDecoderSource (Proxy :: Proxy BankAccountType)
             , toElmDecoderSource (Proxy :: Proxy Customer)
             , toElmDecoderSource (Proxy :: Proxy Application)
             -- : generateElmForAPI  (Proxy :: Proxy Api))
            ]

main :: IO ()
main = do
    specsToDir [spec] "client"
