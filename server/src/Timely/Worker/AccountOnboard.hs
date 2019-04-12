{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Timely.Worker.AccountOnboard where


import           Control.Effects                   (MonadEffects)
import           Control.Effects.Async             (Async)
import qualified Control.Effects.Async             as Async
import           Control.Effects.Early             (Early)
import qualified Control.Effects.Early             as Early
import           Control.Effects.Log               (Log)
import qualified Control.Effects.Log               as Log
import           Control.Effects.Signal            (Throw, throwSignal)
import qualified Control.Effects.Signal            as Signal
import           Control.Effects.Time              (Time, UTCTime (..))
import qualified Control.Effects.Time              as Time
import           Control.Exception                 (Exception)
import           Control.Monad.Catch               (MonadCatch, MonadThrow (..), SomeException (..), catch)
import           Data.Function                     ((&))
import qualified Data.List                         as List
import           Data.Model.Guid                   as Guid
import           Data.Model.Id                     (Id, Token)
import           Data.Model.Types                  (Address (..), Phone, Valid)
import           Data.Text                         as Text
import qualified Database.Selda                    as Selda
import           Network.AMQP.Worker               (Queue)
import qualified Network.AMQP.Worker               as Worker hiding (bindQueue, publish, worker)
import           Timely.Accounts                   (Accounts, TransactionRow)
import qualified Timely.Accounts                   as Accounts
import           Timely.Accounts.Application       (Applications)
import qualified Timely.Accounts.Application       as Applications
import           Timely.Accounts.Types             (Account (..), AppBank, Application (..),
                                                    BankAccount (bankAccountId), Customer (..), Onboarding (..),
                                                    isChecking, toBankAccount)

import qualified Timely.Accounts.Types.Transaction as Transaction
import qualified Timely.App                        as App
import           Timely.Bank                       (Banks, Dwolla, Identity (..), Names (..))
import qualified Timely.Bank                       as Bank
import qualified Timely.Events                     as Events
import           Timely.Transfers                  (AccountInfo (..), TransferAccount, Transfers)
import qualified Timely.Transfers                  as Transfers
import           Timely.Underwriting               (Approval (..), Underwriting (..))
import qualified Timely.Underwriting               as Underwriting



queue :: Queue Application
queue = Worker.topic Events.applicationsNew "app.account.onboard"


start :: IO ()
start = App.start queue handler



handler
  :: ( MonadEffects '[Time, Applications, Accounts, Log, Banks, Transfers, Underwriting, Async] m
     , MonadThrow m, MonadCatch m
     )
  => Application -> m ()
handler app@(Application { accountId, phone }) = do
    Log.context (Guid.toText accountId)
    Log.info "AccountOnboard"

    accountOnboard app accountId phone
      & Early.handleEarly
      & Signal.handleException onError
      & flip catch onException

  where

    onError :: (MonadEffects '[Applications] m, MonadThrow m) => OnboardError -> m ()
    onError err = onException (SomeException err)

    onException :: (MonadEffects '[Applications] m, MonadThrow m) => SomeException -> m ()
    onException err = do
      Applications.markResultOnboarding accountId Error
      throwM err



-- | accountOnboard
accountOnboard
  :: ( MonadEffects '[Applications, Accounts, Log, Banks, Transfers, Underwriting, Throw OnboardError, Time, Early (), Async] m)
  => Application -> Guid Account -> Valid Phone -> m ()
accountOnboard app accountId phone = do
    now                      <- Time.currentTime
    (bankToken, bankItemId)  <- Bank.authenticate (publicBankToken app)
    appBankId                <- Applications.saveBank accountId bankItemId
    customer                 <- newCustomer app now bankToken
    (Approval amount)        <- underwriting accountId customer
    (checking, bankAccounts) <- loadBankAccounts accountId bankToken now
    transId                  <- initTransfers customer bankToken checking

    trans                    <- loadTransactions accountId bankToken appBankId checking now

    let account = Account accountId phone transId bankToken bankItemId amount now
    Accounts.create account customer bankAccounts trans

    Applications.markResultOnboarding accountId Complete
    Log.info "complete"







newCustomer
  :: ( MonadEffects '[Banks] m )
  => Application -> UTCTime -> Token Bank.Access -> m Customer
newCustomer app now bankToken = do
    idt <- Bank.loadIdentity bankToken
    pure $ toNewCustomer now app idt

  where
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


underwriting
  :: ( MonadEffects '[Underwriting, Log, Applications, Early ()] m )
  => Guid Account -> Customer -> m Approval
underwriting accountId cust = do
    res <- Underwriting.newCustomer cust
    Log.debug ("underwriting", res)
    Applications.saveResult accountId res
    case res of
      Underwriting.Denied  _ ->
        Early.earlyReturn ()
      Underwriting.Approved approval ->
        pure approval



loadBankAccounts
  :: ( MonadEffects '[Banks, Log, Throw OnboardError] m )
  => Guid Account -> Token Bank.Access -> UTCTime -> m (BankAccount, [BankAccount])
loadBankAccounts accountId bankToken now = do
    Log.info "load bank accounts"
    banks <- Bank.loadAccounts bankToken
    let bankAccounts = List.map (toBankAccount accountId now) banks
    checking <- require MissingChecking $ List.find isChecking bankAccounts
    pure (checking, bankAccounts)


initTransfers
  :: ( MonadEffects '[Banks, Log, Transfers] m )
  => Customer -> Token Bank.Access -> BankAccount -> m (Id TransferAccount)
initTransfers cust bankToken checking = do
    Log.info "init transfers"
    achTok <- Bank.getACH bankToken (bankAccountId checking)
    transId <- Transfers.createAccount $ toTransferAccountInfo achTok cust
    Log.debug ("transfers", transId)
    pure transId



loadTransactions
  :: ( MonadEffects '[Banks, Applications, Log, Async] m )
  => Guid Account -> Token Bank.Access -> Id AppBank -> BankAccount -> UTCTime -> m [ TransactionRow ]
loadTransactions accountId bankToken appBankId checking (UTCTime today _) = do
    Log.info "load transactions"
    -- Waits for the webhook to update the transactions before continuing
    Async.poll second $ Applications.findTransactions appBankId

    ts <- Bank.loadTransactionsDays bankToken (bankAccountId checking) year today
    Log.debug ("transactions", List.length ts)
    pure $ List.map (Transaction.fromBank accountId) ts
  where
    days = 1
    year = 365 * days
    us   = 1
    ms   = 1000*us
    second = 1000*ms





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




require :: MonadEffects '[Throw OnboardError] m => OnboardError -> (Maybe a) -> m a
require err Nothing = throwSignal err
require _ (Just a)  = pure a
