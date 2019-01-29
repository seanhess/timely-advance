{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Authy where


import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Servant.Client (ClientM, client)


type AuthyApi = VerificationStart :<|> VerificationCheck


data Via = Sms

instance ToHttpApiData Via where
  toQueryParam Sms = "sms"

data Locale = En

instance ToHttpApiData Locale where
  toQueryParam En = "en"

type Phone = Text
type CountryCode = Text
type Code = Text


usa :: CountryCode
usa = "1"

type ParamCountry = QueryParam "country_code" CountryCode
type ParamVia     = QueryParam "via" Via
type ParamPhone   = QueryParam "phone_number" Phone
type ParamLocale  = QueryParam "locale" Locale

type VerificationStart
    = "protected" :> "json" :> "phones" :> "verification" :> "start"
        :> Header "X-Authy-API-Key" AuthyApiKey
        :> ParamVia
        :> ParamCountry
        :> ParamPhone
        :> ParamLocale
        :> Post '[JSON] Response


type ParamCode = QueryParam "verification_code" Code

type VerificationCheck
    = "protected" :> "json" :> "phones" :> "verification" :> "check"
        :> Header "X-Authy-API-Key" AuthyApiKey
        :> ParamCountry
        :> ParamPhone
        :> ParamCode
        :> Get '[JSON] Response


data Response = Response
    { success :: Bool
    } deriving (Show, Generic, Eq)

instance ToJSON   Response
instance FromJSON Response


type AuthyApiKey = Text


api :: Proxy AuthyApi
api = Proxy


callVerifyStart :: (Maybe AuthyApiKey) -> (Maybe Via) -> (Maybe CountryCode) -> (Maybe Phone) -> (Maybe Locale) -> ClientM Response
callVerifyCheck :: (Maybe AuthyApiKey) -> (Maybe CountryCode) -> (Maybe Phone) -> (Maybe Code) -> ClientM Response
callVerifyStart :<|> callVerifyCheck = client api




reqVerifyStart :: AuthyApiKey -> Phone -> ClientM Response
reqVerifyStart key phone = callVerifyStart (Just key) (Just Sms) (Just usa) (Just phone) (Just En)


reqVerifyCheck :: AuthyApiKey -> Phone -> Code -> ClientM Response
reqVerifyCheck key phone code = callVerifyCheck (Just key) (Just usa) (Just phone) (Just code)


-- https://api.authy.com/protected/json/sms/verification/start?via=sms&country_code=1&phone_number=8014189376&locale=en
-- X-Authy-API-Key: xxx

-- 200
-- {
--     "carrier": "Google (Grand Central) BWI - Bandwidth.com - SVR",
--     "is_cellphone": false,
--     "message": "Text message sent to +1 801-418-9376.",
--     "seconds_to_expire": 599,
--     "uuid": "b02ce6b0-fd79-0136-c71b-0e93255c39da",
--     "success": true
-- }

-- 400
-- {
--     "error_code": "60033",
--     "message": "Phone number is invalid",
--     "errors": {
--         "message": "Phone number is invalid"
--     },
--     "success": false
-- }


-- https://api.authy.com/protected/json/phones/verification/check?country_code=1&phone_number=8014189376&verification_code=1298
-- X-Authy-API-Key: xxx

-- 200
-- {
--     "message": "Verification code is correct.",
--     "success": true
-- }

-- 401
-- {
--     "error_code": "60022",
--     "message": "Verification code is incorrect",
--     "errors": {
--         "message": "Verification code is incorrect"
--     },
--     "success": false
-- }
