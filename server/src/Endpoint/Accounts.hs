{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Endpoint.Accounts where

import Control.Monad.State (MonadState, get, put, modify, gets)
import Control.Monad.Except (throwError, MonadError)
import Types.Account (Account(..), AccountInfo(..))
import Data.String.Conversions (cs)
import Data.List (find)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Servant.API.Generic ((:-), ToServantApi, genericApi, toServant)
import Servant.Server.Generic (AsServerT, genericServe)



-- see now we've arrived back at the model or data store
-- sort of.. actually, no
-- we just have business logic
-- not sure where to put this stuff
-- but it should be the main entry point


allAccounts :: (MonadState [Account] m) => m [Account]
allAccounts = get


newAccount :: (MonadState [Account] m) => AccountInfo -> m Account
newAccount a = do
  i <- newId
  let account = Account i a
  modify $ (account:)
  return account


getAccount :: (MonadState [Account] m) => Text -> m (Maybe Account)
getAccount i =
  gets $ find (isAccount i)


saveAccount :: (MonadState [Account] m) => Text -> AccountInfo -> m (Account)
saveAccount i ai = do
  let new = Account i ai
  modify $ update (isAccount i) (\_ -> new)
  return new


newId :: MonadState [Account] m => m Text
newId = gets (cs.show.length)


isAccount i a = accountId a == i

update p f = map each
  where
    each a
      | p a = f a
      | otherwise = a
