{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Worker.OnboardAccount where


import Bank (Banks)
import qualified Bank 
import qualified Events
import qualified AccountStore.Account as Account
import AccountStore.Account (AccountStore)
import AccountStore.Types (Account(..), Customer(..), BankAccount(..), Application(..), BankAccountType(..), balanceFromFloat)
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
import Types.Guid (Guid)



queue :: Queue Application
queue = Worker.topic Events.applicationsNew "app.onboardAccount"



-- TODO effects constraints, not this
handler :: (MonadIO m, Effect m Banks, Effect m AccountStore) => Message Application -> m ()
handler m = do
    let app = value m
        aid = accountId (app :: Application)
    liftIO $ putStrLn "NEW APPLICATION :)"
    liftIO $ print app

    -- get the bank token
    tok <- run $ Bank.Authenticate (publicBankToken app)

    -- create the new account
    run $ Account.CreateAccount app tok


    -- TODO move to bank service
    -- TODO save balance to account
    -- TODO save plaid token to account (better data model?)

    accounts <- run $ Bank.LoadAccounts tok
    let bankAccounts = map (toBankAccount aid) accounts

    -- save the bank accounts
    run $ Account.SetBankAccounts aid bankAccounts


    liftIO $ putStrLn " - done"


toBankAccount :: Guid Account -> Bank.Account -> BankAccount
toBankAccount accountId acc = BankAccount {..}
  where
    id = Selda.def
    accountType
      | Bank._type acc == Bank.Credit = Credit
      | Bank._type acc == Bank.Depository = Savings
      | Bank._type acc == Bank.Loan = Credit
      | Bank.subtype acc == Bank.Checking = Checking
      | Bank.subtype acc == Bank.Savings = Savings
      | otherwise = Other
    name = Bank.name acc
    balance = toBalance $ Bank.current $ Bank.balances acc
    toBalance (Bank.Currency d) = balanceFromFloat d


-- newBank :: Application -> Bank
-- newBank Application {..} = Bank {..}
--   where
--     balance = 200
--     accessToken = "fake-access-token"









