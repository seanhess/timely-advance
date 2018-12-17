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
import Types.AccountInfo (AccountInfo)



instance ElmType Account
instance ElmType AccountInfo
instance ElmType UUID where
    toElmType u = toElmType $ UUID.toText u

-- data Book = Book
--     { name :: String
--     } deriving (Generic)

-- instance ElmType Book

-- type BooksApi = "books" :> Capture "bookId" Int :> Get '[JSON] Book

spec :: Spec
spec = Spec ["Nimble", "Server"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Account)
             : toElmTypeSource    (Proxy :: Proxy AccountInfo)
             : toElmDecoderSource (Proxy :: Proxy Account)
             : toElmEncoderSource (Proxy :: Proxy AccountInfo)
             : toElmDecoderSource (Proxy :: Proxy AccountInfo)
             : generateElmForAPI  (Proxy :: Proxy Api))

main :: IO ()
main = do
    specsToDir [spec] "client"
