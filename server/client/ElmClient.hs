{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ElmClient where

import           Elm                       (ElmType (..), Spec (Spec), specsToDir, toElmDecoderSource,
                                            toElmEncoderSource, toElmTypeSource)
import           Servant.Elm               (Proxy (Proxy), defElmImports)

import           Data.Text                 (Text)
import qualified Data.UUID                 as UUID
import           Database.Selda            (ID, fromId)
import           Network.Plaid.Types       (Id (..), Token (..))
-- import           Text.Megaparsec           (Parsec, parse)

import           Timely.Accounts.Types
import           Timely.Advances           (Advance)
import           Timely.Api.Types          (AccountInfo)
import           Timely.Auth               (Phone)
import           Timely.Types.Guid
import           Timely.Types.Money
import           Timely.Types.Private      (Private)
import           Timely.Underwriting.Types
import           Timely.Types.Session



instance ElmType (Private a) where
    toElmType _ = toElmType (Nothing :: Maybe ())

instance ElmType (ID a) where
    toElmType i = toElmType $ fromId i

instance ElmType (Digits a) where
    toElmType (Digits t) = toElmType t

instance ElmType Account
instance ElmType Application

-- just make these stringy
instance ElmType (Token a) where
    toElmType _ = toElmType (undefined :: Text)

instance ElmType (Id a) where
    toElmType _ = toElmType (undefined :: Text)

instance ElmType (Guid a) where
    toElmType i = toElmType $ UUID.toText i

instance ElmType BankAccount
instance ElmType BankAccountType
instance ElmType Money
instance ElmType Phone
instance ElmType Health
instance ElmType Customer
instance ElmType AccountInfo
instance ElmType Advance
instance ElmType Session
instance ElmType Result
instance ElmType Approval
instance ElmType Denial
instance ElmType DenialReason


spec :: Spec
spec = Spec ["Timely", "Api", "Server"]
            [ defElmImports
            , "import Timely.Api.Id as Id"
            , "import Timely.Api.Types exposing (..)"
            , "import Time"
            , "import Iso8601"
            , toElmTypeSource    (Proxy :: Proxy Account)
            , toElmTypeSource    (Proxy :: Proxy AccountInfo)
            , toElmTypeSource    (Proxy :: Proxy Application)
            , toElmTypeSource    (Proxy :: Proxy BankAccount)
            , toElmTypeSource    (Proxy :: Proxy BankAccountType)
            , toElmTypeSource    (Proxy :: Proxy Customer)
            , toElmTypeSource    (Proxy :: Proxy Health)
            , toElmTypeSource    (Proxy :: Proxy Advance)
            , toElmTypeSource    (Proxy :: Proxy Session)
            , toElmTypeSource    (Proxy :: Proxy Result)
            , toElmTypeSource    (Proxy :: Proxy Approval)
            , toElmTypeSource    (Proxy :: Proxy Denial)
            , toElmTypeSource    (Proxy :: Proxy DenialReason)

            , toElmEncoderSource (Proxy :: Proxy AccountInfo)
            -- , toElmEncoderSource (Proxy :: Proxy (Token a))

            , toElmDecoderSource (Proxy :: Proxy AccountInfo)
            , toElmDecoderSource (Proxy :: Proxy Account)
            , toElmDecoderSource (Proxy :: Proxy BankAccount)
            , toElmDecoderSource (Proxy :: Proxy BankAccountType)
            , toElmDecoderSource (Proxy :: Proxy Customer)
            , toElmDecoderSource (Proxy :: Proxy Session)
            , toElmDecoderSource (Proxy :: Proxy Application)
            , toElmDecoderSource (Proxy :: Proxy Health)
            , toElmDecoderSource (Proxy :: Proxy Advance)
            , toElmDecoderSource (Proxy :: Proxy Result)
            , toElmDecoderSource (Proxy :: Proxy Approval)
            , toElmDecoderSource (Proxy :: Proxy Denial)
            , toElmDecoderSource (Proxy :: Proxy DenialReason)
            ]

write :: IO ()
write = do
    specsToDir [spec] "client"
