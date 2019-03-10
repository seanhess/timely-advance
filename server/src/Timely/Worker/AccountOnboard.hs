{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Timely.Worker.AccountOnboard where


import           Control.Effects                 (MonadEffects)
import           Control.Effects.Log             (Log)
import qualified Control.Effects.Log             as Log
import           Control.Effects.Time            (Time, UTCTime)
import qualified Control.Effects.Time            as Time
import           Control.Exception               (Exception)
import           Control.Monad.Catch             (MonadCatch, MonadThrow (..), SomeException, try)
import qualified Data.List                       as List
import           Data.Model.Guid                 as Guid
import           Data.Model.Id                   (Token)
import           Data.Model.Types                (Address (..), Phone, Valid)
import           Data.Text                       as Text
import qualified Database.Selda                  as Selda
import           Network.AMQP.Worker             (Queue)
import qualified Network.AMQP.Worker             as Worker hiding (bindQueue, publish, worker)
import           Timely.AccountStore.Account     (Accounts)
import qualified Timely.AccountStore.Account     as Account
import           Timely.AccountStore.Application (Applications)
import qualified Timely.AccountStore.Application as Application
import           Timely.AccountStore.Types       (Account, Application (..), BankAccount (balance, bankAccountId),
                                                  Customer (..), Onboarding (..), isChecking, toBankAccount)
import           Timely.Bank                     (Banks, Dwolla, Identity (..), Names (..))
import qualified Timely.Bank                     as Bank
import qualified Timely.Evaluate.AccountHealth   as AccountHealth
import qualified Timely.Events                   as Events
import           Timely.Transfers                (AccountInfo (..), Transfers)
import qualified Timely.Transfers                as Transfers
import           Timely.Underwriting             as Underwriting (Approval (..), Result (..), Underwriting (..), newCustomer)



queue :: Queue Application
queue = Worker.topic Events.applicationsNew "app.account.onboard"





handler
  :: ( MonadEffects '[Time, Applications, Accounts, Log, Banks, Transfers, Underwriting] m
     , MonadThrow m, MonadCatch m
     )
  => Application -> m ()
handler app = do
    let aid = accountId (app :: Application)
    Log.context (Guid.toText aid)
    let phn = phone (app :: Application)

    Log.info "AccountOnboard"


    tok <- Bank.authenticate (publicBankToken app)
    idt <- Bank.loadIdentity tok

    now <- Time.currentTime
    let cust = toNewCustomer now app idt

    res <- Underwriting.newCustomer cust
    Log.debug ("underwriting", res)

    Application.saveResult aid res

    case res of
      Underwriting.Denied   _ -> do
        -- we're done. The user can see their status by polling
        -- liftIO $ putStrLn "DENIED"
        pure ()

      Underwriting.Approved approval -> do
        res <- try $ onboardAccount aid tok cust phn approval

        case res of
          Left (err :: SomeException) -> do
            Application.markResultOnboarding aid Error
            throwM err
          Right _ -> do
            Application.markResultOnboarding aid Complete
            Log.info "done"



onboardAccount
  :: ( MonadEffects '[Time, Accounts, Log, Banks, Transfers] m
     , MonadThrow m
     )
  => Guid Account -> Token Bank.Access -> Customer -> Valid Phone -> Approval -> m ()
onboardAccount aid tok cust phone (Approval amt) = do
    -- get bank accounts
    now <- Time.currentTime
    banks <- Bank.loadAccounts tok
    let bankAccounts = List.map (toBankAccount aid now) banks
    checking <- require MissingChecking $ List.find isChecking bankAccounts

    -- initialize the transfers account
    -- TODO this is very likely to error atm. Duplicate emails
    achTok <- Bank.getACH tok (bankAccountId checking)
    transId <- Transfers.createAccount $ toTransferAccountInfo achTok cust

    -- TODO the transactions might not be available until the webhook triggers - maybe it makes more sense to have the health in a pending state. Or just: We're good!
    let health = AccountHealth.analyze (balance checking)
    Account.create $ Account.account aid now phone cust tok amt health transId

    -- save the bank accounts
    -- TODO make it impossible to forget this
    Account.setBanks aid bankAccounts




  where
    require :: (MonadThrow m, Exception err) => err -> (Maybe a) -> m a
    require err Nothing = throwM err
    require _ (Just a)  = pure a

toNewCustomer :: UTCTime -> Application -> Identity -> Customer
toNewCustomer now Application {accountId, email, dateOfBirth, ssn} Identity {names, address} =
  let Address {street1, street2, city, state, postalCode } = address
      Names {firstName, middleName, lastName} = names
  in Customer
      { accountId, email, id = Selda.def
      , firstName, middleName, lastName
      , ssn, dateOfBirth
      , street1, street2, city, state, postalCode
      , created = now
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
