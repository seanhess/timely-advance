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
import AccountStore.Types (Account(..), BankAccount(..), Application(..), BankAccountType(..), balanceFromFloat)
import Control.Monad.Service (Service(run))
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Database.Selda as Selda
import Network.AMQP.Worker (Queue)
import qualified Network.AMQP.Worker as Worker hiding (publish, bindQueue, worker)
import Types.Guid (Guid)



queue :: Queue Application
queue = Worker.topic Events.applicationsNew "app.onboardAccount"



-- TODO effects constraints, not this
handler :: (MonadIO m, Service m Banks, Service m AccountStore) => Application -> m ()
handler app = do
    let aid = accountId (app :: Application)
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









