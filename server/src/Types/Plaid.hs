module Types.Plaid
  ( Token(..)
  , Access
  ) where


import Database.Selda.SqlType (Lit(..), SqlType(..))
import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Network.Plaid.Types (Token(..), Access)

-- I want this to act like text
instance Typeable t => SqlType (Token t) where
    mkLit (Token t) =  LCustom $ mkLit t
    sqlType _ = sqlType (Proxy :: Proxy Text)
    fromSql v = Token $ fromSql v
    defaultValue = LCustom $ (defaultValue :: Lit Text)


