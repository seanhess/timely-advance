{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Worker.OnboardAccount where


import Control.Monad.Service (Service(run))
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Database.Selda as Selda
import Network.AMQP.Worker (Queue)
import qualified Network.AMQP.Worker as Worker hiding (publish, bindQueue, worker)

import           Bank (Banks)
import qualified Bank 
import           Underwriting (Underwriting(..), Result(..))
import qualified Events
import           AccountStore.Application (ApplicationStore)
import qualified AccountStore.Application as Application
import           AccountStore.Account (AccountStore)
import qualified AccountStore.Account as Account
import           AccountStore.Types (Account(..), BankAccount(..), Application(..), BankAccountType(..))

import Types.Guid (Guid)
import Types.Money as Money



queue :: Queue Application
queue = Worker.topic Events.applicationsNew "app.onboardAccount"



-- TODO effects constraints, not this
handler
  :: ( MonadIO m
     , Service m Banks
     , Service m AccountStore
     , Service m Underwriting
     , Service m ApplicationStore
     )
  => Application -> m ()
handler app = do
    let aid = accountId (app :: Application)
    liftIO $ putStrLn "NEW APPLICATION :)"
    liftIO $ print app

    tok <- run $ Bank.Authenticate (publicBankToken app)

    res <- run $ Underwriting.New app

    run $ Application.SaveResult aid res


    case res of
      Underwriting.Denied   _ -> do
        -- we're done. The user can see their status by polling
        liftIO $ putStrLn "DENIED"

      Underwriting.Approved _ -> do
        liftIO $ putStrLn "APPROVED"

        -- sure...
        run $ Account.CreateAccount $ Account.account app tok

        -- save the bank accounts
        accounts <- run $ Bank.LoadAccounts tok
        -- liftIO $ print accounts

        let bankAccounts = map (toBankAccount aid) accounts
        -- liftIO $ putStrLn " ----- Banks----------------------- "
        -- liftIO $ print bankAccounts
        run $ Account.SetBankAccounts aid bankAccounts




toBankAccount :: Guid Account -> Bank.Account -> BankAccount
toBankAccount accountId acc = BankAccount {..}
  where
    id = Selda.def
    accountType
      | Bank.subtype acc == Bank.Checking = Checking
      | Bank.subtype acc == Bank.Savings = Savings
      | Bank._type acc == Bank.Credit = Credit
      | Bank._type acc == Bank.Depository = Savings
      | Bank._type acc == Bank.Loan = Credit
      | otherwise = Other
    name = Bank.name acc
    balance = toBalance $ Bank.current $ Bank.balances acc
    toBalance (Bank.Currency d) = Money.fromFloat d
    bankAccountId = Bank.account_id acc


-- newBank :: Application -> Bank
-- newBank Application {..} = Bank {..}
--   where
--     balance = 200
--     accessToken = "fake-access-token"









