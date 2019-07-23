{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Experian.CrossCore where

import Crypto.Hash.Algorithms  (SHA256)
import Crypto.MAC.HMAC         (HMAC (hmacGetDigest), hmac)
import Data.ByteArray          (ByteArrayAccess)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString         (ByteString)

-- {"key":"value"}

newtype HMACSecret = HMACSecret ByteString
  deriving (ByteArrayAccess)

signature :: HMACSecret -> ByteString -> HMAC SHA256
signature = hmac

toString :: HMAC SHA256 -> ByteString
toString = convertToBase Base64 . hmacGetDigest

