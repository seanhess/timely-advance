module Timely.Underwrite.Experian
  where


import Control.Concurrent.MVar                 (MVar, putMVar, takeMVar)
import Control.Exception                       (Exception, throwIO, catch)
import Control.Monad.IO.Class                  (MonadIO, liftIO)
import Data.Aeson                              as Aeson (Result (..), Value, encode, fromJSON)
import Data.ByteString.Lazy                    (ByteString)
import Data.List                               as List (find)
import Data.Maybe                              (listToMaybe)
import Data.Text                               (Text)
import Network.Experian.CreditProfile          as CreditProfile (AccessToken, Credentials, authenticate, load, checkUnauthorized, AuthError(..))
import Network.Experian.CreditProfile.Request  (Request (..))
import Network.Experian.CreditProfile.Response (CreditProfile (..), Response (..), RiskModel (..))
import Timely.Accounts.Types.Customer          (Customer)


-- TODO warehouse full data set

newtype VantageScore = VantageScore Text
  deriving (Show)


data Config = Config
  { endpoint    :: String
  , credentials :: Credentials
  }



data Error
  = JSONError String ByteString
  | NoScore ByteString
  deriving (Show)
instance Exception Error


-- 0. Authenticate on startup, setting the mvar
-- 1. takeMVar - blocks until loaded
-- 2. attempt to load
-- 3. on auth failure, authenticate and repeat the request once (the inner function that actually loads without re-authenticating)
-- 4. putMVar - return the token that works to the mvar
vantageScore :: MonadIO m => Config -> MVar AccessToken -> Customer -> m VantageScore
vantageScore (Config endpoint _) tokVar customer = do
  tok <- liftIO $ takeMVar tokVar

  eval <- CreditProfile.load endpoint tok (toRequest customer)
  case eval of
    Left (Unauthorized _) -> error "NEED TO REAUTH"
    Right val -> do
      liftIO $ putMVar tokVar tok
      parseVantageScore val



toRequest :: Customer -> Request
toRequest = undefined



-- it should throw an error if the vantage score isn't obtainable?
parseVantageScore :: MonadIO m => Value -> m VantageScore
parseVantageScore val = do
  case fromJSON val of
    Error err -> liftIO $ throwIO $ JSONError err (encode val)
    Success res ->
      case findVantageScore res of
        Nothing -> liftIO $ throwIO $ NoScore (encode val)
        Just s  -> pure s


findVantageScore :: Response -> Maybe VantageScore
findVantageScore res = do
    p <- listToMaybe $ creditProfile res
    m <- List.find (\rm -> modelIndicator rm == "V4") $ riskModel p
    pure $ VantageScore $ score m
