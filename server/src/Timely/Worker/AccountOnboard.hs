{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Timely.Worker.AccountOnboard where


import           Control.Exception               (Exception)
import           Control.Monad.Catch             (MonadThrow (..), try, MonadCatch, SomeException)
import           Control.Monad.Log               (MonadLog)
import qualified Control.Monad.Log               as Log
import           Control.Monad.Service           (Service (run))
import qualified Data.List                       as List
import           Data.Model.Guid                 as Guid
import           Data.Model.Id                   (Token)
import           Data.Model.Types                (Address (..), Valid, Phone)
import           Data.Text                       as Text
import qualified Database.Selda                  as Selda
import           Network.AMQP.Worker             (Queue)
import qualified Network.AMQP.Worker             as Worker hiding (bindQueue, publish, worker)
import           Timely.AccountStore.Account     (AccountStore)
import qualified Timely.AccountStore.Account     as Account
import           Timely.AccountStore.Application (ApplicationStore)
import qualified Timely.AccountStore.Application as Application
import           Timely.AccountStore.Types       (Application (..), BankAccount (balance, bankAccountId), Customer (..),
                                                  Onboarding (..), isChecking, toBankAccount, Account)
import           Timely.Bank                     (Banks, Dwolla, Identity (..), Names (..))
import qualified Timely.Bank                     as Bank
import qualified Timely.Evaluate.AccountHealth   as AccountHealth
import qualified Timely.Events                   as Events
import           Timely.Transfers                (AccountInfo (..), Transfers)
import qualified Timely.Transfers                as Transfers
import           Timely.Underwriting             as Underwriting (Approval (..), Result (..), Underwriting (..))



queue :: Queue Application
queue = Worker.topic Events.applicationsNew "app.account.onboard"





handler
  :: ( Service m Banks
     , Service m AccountStore
     , Service m Underwriting
     , Service m ApplicationStore
     , Service m Transfers
     , MonadThrow m, MonadCatch m
     , MonadLog m
     )
  => Application -> m ()
handler app = do
    let aid = accountId (app :: Application)
    Log.context (Guid.toText aid)
    let phn = phone (app :: Application)

    Log.info "AccountOnboard"

    tok <- run $ Bank.Authenticate (publicBankToken app)
    idt <- run $ Bank.LoadIdentity tok

    let cust = toNewCustomer app idt

    res <- run $ Underwriting.New cust
    Log.debug ("underwriting", res)

    run $ Application.SaveResult aid res

    case res of
      Underwriting.Denied   _ -> do
        -- we're done. The user can see their status by polling
        -- liftIO $ putStrLn "DENIED"
        pure ()

      Underwriting.Approved approval -> do
        res <- try $ onboardAccount aid tok cust phn approval

        case res of
          Left (err :: SomeException) -> do
            run $ Application.MarkResultOnboarding aid Error
            throwM err
          Right _ -> do
            run $ Application.MarkResultOnboarding aid Complete
            Log.info "done"



onboardAccount
  :: ( Service m Banks
     , Service m AccountStore
     , Service m ApplicationStore
     , Service m Transfers
     , MonadThrow m
     , MonadCatch m
     , MonadLog m
     )
  => Guid Account -> Token Bank.Access -> Customer -> Valid Phone -> Approval -> m ()
onboardAccount aid tok cust phone (Approval amt) = do
    -- get bank accounts
    banks <- run $ Bank.LoadAccounts tok
    let bankAccounts = List.map (toBankAccount aid) banks
    checking <- require MissingChecking $ List.find isChecking bankAccounts

    -- initialize the transfers account
    -- TODO this is very likely to error atm. Duplicate emails
    achTok <- run $ Bank.GetACH tok (bankAccountId checking)
    transId <- run $ Transfers.CreateAccount $ toTransferAccountInfo achTok cust

    -- TODO the transactions might not be available until the webhook triggers - maybe it makes more sense to have the health in a pending state. Or just: We're good!
    let health = AccountHealth.analyze (balance checking)
    run $ Account.CreateAccount $ Account.account aid phone cust tok amt health transId

    -- save the bank accounts
    -- TODO make it impossible to forget this
    run $ Account.SetBankAccounts aid bankAccounts




  where
    require :: (MonadThrow m, Exception err) => err -> (Maybe a) -> m a
    require err Nothing = throwM err
    require _ (Just a)  = pure a

toNewCustomer :: Application -> Identity -> Customer
toNewCustomer Application {accountId, email, dateOfBirth, ssn} Identity {names, address} =
  let Address {street1, street2, city, state, postalCode } = address
      Names {firstName, middleName, lastName} = names
  in Customer
      { accountId, email, id = Selda.def
      , firstName, middleName, lastName
      , ssn, dateOfBirth
      , street1, street2, city, state, postalCode
      }


toTransferAccountInfo :: Token Dwolla -> Customer -> Transfers.AccountInfo
toTransferAccountInfo processorToken Customer { accountId, firstName, lastName, email, dateOfBirth, ssn, street1, street2, city, state, postalCode } =
  AccountInfo
    { accountId, email
    , firstName, lastName
    , dateOfBirth, ssn
    , processorToken
    , address = Address { street1, street2, city, state, postalCode }
    }












data OnboardError
    = BadName Text
    | NoNames
    | MissingChecking
    deriving (Eq, Show)

instance Exception OnboardError
