{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Worker.OnboardAccount where


import Bank (Banks)
import qualified Bank 
import qualified Events
import qualified AccountStore.Account as Account
import AccountStore.Account (AccountStore)
import AccountStore.Types (Account(..), Customer(..), BankAccount(..), Application(..))
import Control.Exception (SomeException(..))
import Control.Monad.Effect (Effect(run))
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Plaid (MonadPlaid, runPlaid)
import qualified Control.Monad.Plaid as Plaid
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Database.Selda as Selda
import Database.Selda.PostgreSQL (pgOpen, PGConnectInfo(..))
import Network.AMQP.Worker (key, word, Key, Routing, WorkerException, def, Message(..))
import Network.AMQP.Worker (Queue)
import qualified Network.AMQP.Worker as Worker hiding (publish, bindQueue, worker)
import qualified Network.AMQP.Worker.Monad as Worker
import Network.AMQP.Worker.Monad (MonadWorker(..))
import qualified Network.Plaid as Plaid
import qualified Network.Plaid.Types as Plaid
import Network.Plaid.Types (ExchangeTokenResponse(..))
import Database.Selda.Backend (SeldaConnection, MonadSelda(..), SeldaT, runSeldaT)
import Worker.WorkerM (WorkerM, loadState)
import Types.Private (private)



queue :: Queue Application
queue = Worker.topic Events.applicationsNew "app.onboardAccount"



-- TODO effects constraints, not this
handler :: (MonadIO m, Effect m Banks, Effect m AccountStore) => Message Application -> m ()
handler m = do
    let app = value m
    liftIO $ putStrLn "NEW APPLICATION :)"
    liftIO $ print app

    -- create the customer record
    run $ Account.CreateCustomer (newCustomer app)


    -- TODO move to bank service
    -- TODO save balance to account
    -- TODO save plaid token to account (better data model?)

    tok <- run $ Bank.Authenticate (publicBankToken app)
    accounts <- run $ Bank.LoadAccounts tok
    liftIO $ mapM_ print accounts

    -- creds <- Plaid.credentials
    -- ExchangeTokenResponse { access_token } <- runPlaid $ Plaid.reqExchangeToken creds (plaidToken app)
    -- res <- runPlaid $ Plaid.reqAccounts creds access_token
    -- liftIO $ print $ res

    liftIO $ putStrLn " - done"


newCustomer :: Application -> Customer
newCustomer Application {..} = Customer {..}
  where id = Selda.def

-- newBank :: Application -> Bank
-- newBank Application {..} = Bank {..}
--   where
--     balance = 200
--     accessToken = "fake-access-token"









