{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Timely.Worker.OnboardAccount where


import Control.Monad.Service (Service(run))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Exception (Exception, throwIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Database.Selda as Selda
import Network.AMQP.Worker (Queue)
import qualified Network.AMQP.Worker as Worker hiding (publish, bindQueue, worker)

import           Timely.Bank (Banks)
import qualified Timely.Bank as Bank
import           Timely.Underwriting as Underwriting (Underwriting(..), Result(..))
import qualified Timely.Events as Events
import           Timely.AccountStore.Application (ApplicationStore)
import qualified Timely.AccountStore.Application as Application
import           Timely.AccountStore.Account (AccountStore)
import qualified Timely.AccountStore.Account as Account
import           Timely.AccountStore.Types (Account(..), BankAccount(..), Application(..), BankAccountType(..), Customer(..))
import           Timely.Types.Guid (Guid)
import           Timely.Types.Money as Money



queue :: Queue Application
queue = Worker.topic Events.applicationsNew "app.onboardAccount"



data OnboardError
    = BadName Text
    | NoNames
    deriving (Eq, Show)

instance Exception OnboardError


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
    let phn = phone (app :: Application)
    liftIO $ putStrLn "NEW APPLICATION :)"
    liftIO $ print app

    tok <- run $ Bank.Authenticate (publicBankToken app)
    idt <- run $ Bank.LoadIdentity tok

    cust <- toNewCustomer app idt

    res <- run $ Underwriting.New cust

    run $ Application.SaveResult aid res


    case res of
      Underwriting.Denied   _ -> do
        -- we're done. The user can see their status by polling
        liftIO $ putStrLn "DENIED"

      Underwriting.Approved _ -> do
        liftIO $ putStrLn "APPROVED"

        run $ Account.CreateAccount $ Account.account aid phn cust tok

        -- save the bank accounts
        accounts <- run $ Bank.LoadAccounts tok
        -- liftIO $ print accounts

        let bankAccounts = map (toBankAccount aid) accounts
        -- liftIO $ putStrLn " ----- Banks----------------------- "
        -- liftIO $ print bankAccounts
        run $ Account.SetBankAccounts aid bankAccounts



-- TODO don't use IO exceptions
toNewCustomer :: MonadIO m => Application -> Bank.Identity -> m Customer
toNewCustomer Application {..} identity = do
    let id = Selda.def
    (firstName, middleName, lastName) <- parseName
    pure $ Customer {..}
  where
    parseName = do
      case List.map Text.words (Bank.names identity) of
        [f, m, l]:_ -> pure (f, Just m, l)
        [f, l]:_ -> pure (f, Nothing, l)
        n:_ -> liftIO $ throwIO $ BadName $ Text.unwords n
        _ -> liftIO $ throwIO $ NoNames





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









