{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Timely.Worker.AccountOnboard where


import           Control.Monad.Service (Service(run))
import           Control.Monad.Catch (MonadThrow(..))
import           Control.Exception (Exception)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Database.Selda as Selda
import           Network.AMQP.Worker (Queue)
import qualified Network.AMQP.Worker as Worker hiding (publish, bindQueue, worker)

import           Timely.Bank (Banks)
import qualified Timely.Bank as Bank
import           Timely.Underwriting as Underwriting (Underwriting(..), Result(..), Approval(..))
import qualified Timely.Events as Events
import           Timely.AccountStore.Application (ApplicationStore)
import qualified Timely.AccountStore.Application as Application
import           Timely.AccountStore.Account (AccountStore)
import qualified Timely.AccountStore.Account as Account
import           Timely.AccountStore.Types (Application(..), Customer(..), toBankAccount, isChecking, BankAccount(balance))
import qualified Timely.Evaluate.AccountHealth as AccountHealth



queue :: Queue Application
queue = Worker.topic Events.applicationsNew "app.account.onboard"





handler
  :: ( Service m Banks
     , Service m AccountStore
     , Service m Underwriting
     , Service m ApplicationStore
     , MonadThrow m
     )
  => Application -> m ()
handler app = do
    let aid = accountId (app :: Application)
    let phn = phone (app :: Application)
    -- liftIO $ putStrLn "NEW APPLICATION :)"
    -- liftIO $ print app

    tok <- run $ Bank.Authenticate (publicBankToken app)
    idt <- run $ Bank.LoadIdentity tok

    cust <- toNewCustomer app idt

    res <- run $ Underwriting.New cust

    run $ Application.SaveResult aid res


    case res of
      Underwriting.Denied   _ -> do
        -- we're done. The user can see their status by polling
        -- liftIO $ putStrLn "DENIED"
        pure ()

      Underwriting.Approved (Approval amt) -> do
        -- liftIO $ putStrLn "APPROVED"

        -- get bank accounts
        banks <- run $ Bank.LoadAccounts tok
        let bankAccounts = map (toBankAccount aid) banks
        checking <- require MissingChecking $ List.find isChecking bankAccounts

        -- TODO the transactions might not be available until the webhook triggers - maybe it makes more sense to have the health in a pending state. Or just. We're good!
        let health = AccountHealth.analyze (balance checking)
        run $ Account.CreateAccount $ Account.account aid phn cust tok amt health

        -- save the bank accounts
        -- liftIO $ putStrLn " ----- Banks----------------------- "
        -- liftIO $ print bankAccounts
        run $ Account.SetBankAccounts aid bankAccounts

  where
    require :: (MonadThrow m, Exception err) => err -> (Maybe a) -> m a
    require err Nothing = throwM err
    require _ (Just a)  = pure a


toNewCustomer :: MonadThrow m => Application -> Bank.Identity -> m Customer
toNewCustomer Application {..} identity = do
    let id = Selda.def
    (firstName, middleName, lastName) <- parseName
    pure $ Customer {..}
  where
    parseName = do
      case List.map Text.words (Bank.names identity) of
        [f, m, l]:_ -> pure (f, Just m, l)
        [f, l]:_ -> pure (f, Nothing, l)
        n:_ -> throwM $ BadName $ Text.unwords n
        _ -> throwM $ NoNames














data OnboardError
    = BadName Text
    | NoNames
    | MissingChecking
    deriving (Eq, Show)

instance Exception OnboardError
