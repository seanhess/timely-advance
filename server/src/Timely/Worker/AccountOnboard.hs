{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Timely.Worker.AccountOnboard where

import           Control.Applicative               ((<|>))
import           Control.Effects                   (MonadEffects)
import           Control.Effects.Async             (Async)
import qualified Control.Effects.Async             as Async
import           Control.Effects.Early             (Early, earlyReturn)
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
import           Data.Model.Types                  as Model (Address (..), Phone, Valid)
import           Data.Number.Abs                   (value)
import           Data.Text                         as Text
import           Network.AMQP.Worker               (Queue)
import qualified Network.AMQP.Worker               as Worker hiding (bindQueue, publish, worker)
import           Timely.Accounts                   (Accounts, TransactionRow)
import qualified Timely.Accounts                   as Accounts
import           Timely.Accounts.Application       (Applications)
import qualified Timely.Accounts.Application       as Apps
import           Timely.Accounts.Budgets           (Budgets)
import qualified Timely.Accounts.Budgets           as Budgets
import           Timely.Accounts.Subscription      as Subscription (Level (..))
import           Timely.Accounts.Types             as Accounts (Account (..), AppBank, Application (..), BankAccount (bankAccountId), Customer (..), Onboarding (..), Pending (..), Rejected (..), isChecking, toBankAccount)
import qualified Timely.Accounts.Types.Transaction as Transaction
import           Timely.Actions.Transactions       (History (..))
import qualified Timely.Actions.Transactions       as Transactions
import           Timely.App                        as App (AppState, AppT, runApp, start)
import           Timely.Bank                       (Banks, Dwolla, Identity (..), Names (..))
import qualified Timely.Bank                       as Bank
import           Timely.Evaluate.History           as History (transSpansDays, Group(..), monthlyAverage)
import qualified Timely.Events                     as Events
import           Timely.Transfers                  (AccountInfo (..), TransferAccount, Transfers)
import qualified Timely.Transfers                  as Transfers

queue :: Queue Application
queue = Worker.topic Events.applicationsNew "app.account.onboard"


start :: IO ()
start = start' runApp

start' :: (forall a. AppState -> AppT IO a -> IO a) -> IO ()
start' run = App.start run queue handler



handler
  :: ( MonadEffects '[Time, Applications, Accounts, Log, Banks, Transfers, Async, Budgets] m
     , MonadThrow m, MonadCatch m
     )
  => Application -> m ()
handler app@(Application { accountId, phone }) = do
    Log.context (Guid.toText accountId)
    Log.info "AccountOnboard"

    result <- accountOnboard app accountId phone
      & Early.handleEarly
      & Signal.handleException onError
      & flip catch onException

    Apps.updateOnboarding accountId result
    Log.info "complete"

  where

    onError :: (MonadEffects '[Applications] m, MonadThrow m) => OnboardError -> m Onboarding
    onError err = onException (SomeException err)

    onException :: (MonadEffects '[Applications] m, MonadThrow m) => SomeException -> m Onboarding
    onException err = do
      Apps.updateOnboarding accountId Error
      throwM err
      pure Error



-- | accountOnboard
-- we still need to wait for account analysis
accountOnboard
  :: ( MonadEffects '[Applications, Accounts, Log, Banks, Transfers, Throw OnboardError, Time, Async, Budgets, Early Onboarding] m)
  => Application -> Guid Account -> Valid Phone -> m Onboarding
accountOnboard app accountId phone = do

    Apps.updateOnboarding accountId (Pending Bank)
    now                      <- Time.currentTime
    (bankToken, bankItemId)  <- Bank.authenticate (publicBankToken app)
    appBankId                <- Apps.saveBank accountId bankItemId
    customer                 <- newCustomer app now bankToken
    (checking, bankAccounts) <- loadBankAccounts accountId bankToken now

    Apps.updateOnboarding accountId (Pending Transfers)
    transId                  <- initTransfers customer bankToken checking

    Apps.updateOnboarding accountId (Pending Transactions)
    trans                    <- loadTransactions accountId bankToken appBankId checking now

    let history              = Transactions.history trans
    sub                      <- early $ checkMinimalRequirements history

    Apps.updateOnboarding accountId (Pending Creation)
    Accounts.create customer bankAccounts trans sub $ Account accountId phone transId bankToken bankItemId now
    -- we need to collect the first subscription here? Or queue it for collection?

    createDefaultBudgets accountId trans history

    pure Complete




createDefaultBudgets
  :: ( MonadEffects '[Time, Accounts, Budgets, Log] m )
  => Guid Account -> [TransactionRow] -> History -> m ()
createDefaultBudgets accountId trans history = do

  let incs = List.map Transactions.defaultBudget $ Transactions.income history
  let exps = List.map Transactions.defaultBudget $ Transactions.expenses history
  let spend = Transactions.discretionarySpending numDaysHistory exps trans

  Budgets.saveIncomes accountId incs
  Budgets.saveExpenses accountId exps
  Budgets.saveSpending accountId spend

  Log.debug ("BUDGETS", incs, exps, spend)
  pure ()






newCustomer
  :: ( MonadEffects '[Banks] m )
  => Application -> UTCTime -> Token Bank.Access -> m Customer
newCustomer app now bankToken = do
    idt <- Bank.loadIdentity bankToken
    pure $ toNewCustomer now app idt

  where
    toNewCustomer :: UTCTime -> Application -> Identity -> Customer
    toNewCustomer now Application {accountId, email} Identity {names, address} =
      let Address {street1, street2, city, state, postalCode } = address
          Names {firstName, middleName, lastName} = names
      in Customer
          { accountId
          , firstName, middleName, lastName
          , email
          , street1, street2, city, state, postalCode
          , created = now
          }



-- it could be an either instead
checkMinimalRequirements
  :: History -> Either Onboarding Subscription.Level
checkMinimalRequirements history = do
    case check history of
      Just r -> do
        Left $ Rejected r

      Nothing ->
        Right Subscription.Basic

  where

    check history = checkRegular history <|> checkLow history <|> checkShort history

    checkRegular history =
      if isNotRegular (income history)
        then Just $ IncomeNotRegular
        else Nothing

    checkShort history =
      if isTooShort (income history)
        then Just $ IncomeTooShort
        else Nothing

    checkLow history =
      if isLow (income history) (expenses history)
        then Just IncomeLow
        else Nothing

    isLow incs exps =
      primary incs <= groupsTotal exps

    groupsTotal = List.sum . List.map (value . History.monthlyAverage)

    primary = List.maximum . List.map (value . History.monthlyAverage)

    isNotRegular incs =
      List.length incs < 1

    isTooShort []      = True
    isTooShort (inc:_) =
      -- if it's less than two months, say no. 28d + 31d = 59 days
      (History.transSpansDays $ History.transactions inc) < 59


-- underwriting
--   :: ( MonadEffects '[Underwrite, Log, Applications, Early ()] m )
--   => Guid Account -> Valid Phone -> Customer -> m Approval
-- underwriting accountId phone cust = do
--     res <- Underwrite.new $ toApplication phone cust
--     Log.debug ("underwriting", res)
--     Apps.saveResult accountId res
--     case res of
--       Underwrite.Denied  _ ->
--         Early.earlyReturn ()
--       Underwrite.Approved approval ->
--         pure approval
--   where

--     toApplication phone Customer { firstName, middleName, lastName, email, ssn, dateOfBirth, street1, street2, city, state, postalCode } =
--       Underwrite.Application
--         { Underwrite.phone = phone
--         , Underwrite.firstName = firstName
--         , Underwrite.middleName = middleName
--         , Underwrite.lastName = lastName
--         , Underwrite.email = email
--         , Underwrite.ssn = ssn
--         , Underwrite.dateOfBirth = dateOfBirth
--         , Underwrite.address = Address street1 street2 city state postalCode
--         }



loadBankAccounts
  :: ( MonadEffects '[Banks, Log, Throw OnboardError] m )
  => Guid Account -> Token Bank.Access -> UTCTime -> m (BankAccount, [BankAccount])
loadBankAccounts accountId bankToken now = do
    Log.info "load bank accounts"
    banks <- Bank.loadAccounts bankToken
    let bankAccounts = List.map (toBankAccount accountId now) banks
    checking <- require NoChecking $ List.find isChecking bankAccounts
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
    Async.poll second $ Apps.findTransactions appBankId

    ts <- Bank.loadTransactionsDays bankToken (bankAccountId checking) year today
    Log.debug ("transactions", List.length ts)
    pure $ List.map (Transaction.fromBank accountId) ts
  where
    days = 1
    year = numDaysHistory * days
    us   = 1
    ms   = 1000*us
    second = 1000*ms



numDaysHistory :: Integer
numDaysHistory = 365



toTransferAccountInfo :: Token Dwolla -> Customer -> Transfers.AccountInfo
toTransferAccountInfo processorToken Customer { accountId, firstName, lastName, email, street1, street2, city, state, postalCode } =
  AccountInfo
    { accountId, email
    , firstName, lastName
    , processorToken
    , address = Address { street1, street2, city, state, postalCode }
    }












data OnboardError
    = BadName Text
    | NoNames
    | NoChecking
    | NoIncome
    deriving (Eq, Show)

instance Exception OnboardError




require :: MonadEffects '[Throw OnboardError] m => OnboardError -> (Maybe a) -> m a
require err Nothing = throwSignal err
require _ (Just a)  = pure a


early :: MonadEffects '[Early Onboarding] m => Either Onboarding a -> m a
early (Left r)  = earlyReturn r
early (Right a) = pure a
