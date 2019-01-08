{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module TestPlaid where

import Data.String.Conversions
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Plaid
import Network.Plaid.Types
import Servant.Client
import Text.Show.Pretty

main :: IO ()
main = putStrLn "Test suite not yet implemented"

deriving instance PrettyVal AuthResponse
deriving instance PrettyVal AccountsResponse
deriving instance PrettyVal (Id a)
deriving instance PrettyVal Account
deriving instance PrettyVal Balances
deriving instance PrettyVal Currency
deriving instance PrettyVal CurrencyCode
deriving instance PrettyVal AccountType
deriving instance PrettyVal AccountSubType
deriving instance PrettyVal AuthNumbers
deriving instance PrettyVal Ach
deriving instance PrettyVal Eft
deriving instance PrettyVal (Number a)
instance PrettyVal Item where
  prettyVal _ = String "{item}"


test :: IO ()
test = do
    -- putStrLn "Enter public token"
    -- tok <- getLine
    -- putStrLn $ "Using token: " ++ tok

    manager <- newManager tlsManagerSettings
    let env = mkClientEnv manager (BaseUrl Https "sandbox.plaid.com" 443 "")
    let creds = Credentials (Id "5c1a663c5eca930011ff67ee") (Id "db8bad5d68d41340cba767615c7aea")
    let access = Token "access-sandbox-42065f86-d64a-4a9d-abd2-b1226aaf693f"
    -- print =<< runClientM (sendExchangeToken $ ExchangeTokenRequest (Id "5c1a663c5eca930011ff67ee") (Id "db8bad5d68d41340cba767615c7aea") (Token (cs tok))) env

    -- res <- runClientM (sendAuth $ AuthRequest (Id "5c1a663c5eca930011ff67ee") (Id "db8bad5d68d41340cba767615c7aea") (Token "access-sandbox-42065f86-d64a-4a9d-abd2-b1226aaf693f")) env


    res <- runClientM (reqAccounts creds access) env
    case res of
      Left (DecodeFailure t r) -> do
        putStrLn $ cs $ t
        putStrLn $ cs $ responseBody r
      Right b -> do
        putStrLn $ dumpStr b
        -- putStrLn $  b

    pure ()




