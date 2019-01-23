{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Auth where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Except (MonadError)
import           Control.Monad.Service (Service(..))
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Data.Text (Text)

import           Servant.Auth.Server
import           Servant

-- Roles
-- 1. Generate SMS codes
-- 2. Verify SMS codes
-- 3. Provide some auth checking helpers to the rest of the system


newtype Phone = Phone Text
    deriving (Show, Eq, Generic, ToJSON, FromJSON, FromHttpApiData, Typeable)


newtype AuthCode = AuthCode Text
   deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance MimeUnrender PlainText AuthCode where
    mimeUnrender _ s = AuthCode <$> mimeUnrender (Proxy :: Proxy PlainText) s



data AuthService a where
    CodeGenerate :: Phone -> AuthService ()
    CodeCheck    :: Phone -> AuthCode -> AuthService Bool


-- there's an obvious implementation for anyone who has a MonadSelda
instance (MonadIO m) => Service m AuthService where
    run (CodeGenerate _)  = pure ()
    run (CodeCheck (Phone p) (AuthCode c))   =
      pure $ p == c



login :: (ToJWT session, MonadIO m, MonadError ServantErr m) => CookieSettings -> JWTSettings -> session -> value -> m (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie ] value)
login cke jwt session value = do
  mApplyCookies <- liftIO $ acceptLogin cke jwt session
  case mApplyCookies of
    Nothing -> throwError err401
    Just applyCookies -> pure $ applyCookies value


logout :: Applicative m => CookieSettings -> m (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
logout cke =
  pure $ clearSession cke NoContent
