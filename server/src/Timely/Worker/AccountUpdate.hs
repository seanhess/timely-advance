{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Timely.Worker.AccountUpdate where

import           Control.Monad.Service (Service(run))
import           Control.Monad.Except (MonadError(..))
import           Control.Exception (Exception)
import qualified Data.List as List
import qualified Network.AMQP.Worker as Worker

import qualified Timely.Bank as Bank
import           Timely.Bank (Banks, Token, Access)
import qualified Timely.Evaluate.AccountHealth as AccountHealth
import           Timely.Evaluate.AccountHealth (Health(..))
import qualified Timely.AccountStore.Account as AccountStore
import           Timely.AccountStore.Account (AccountStore)
import           Timely.AccountStore.Types (Account(bankToken, credit), BankAccount(accountType, balance), BankAccountType(Checking), toBankAccount)
import           Timely.Types.Guid (Guid)
import           Timely.Types.Private (Private(..))
import           Timely.Events as Events


queue :: Worker.Queue (Guid Account)
queue = Worker.topic Events.transactionsNew "app.account.update"


-- it's not evaluate, its: what do we do on a new transaction
-- AccountAction
-- UpdateAccount
-- Yeah, this is idempotent. You can run it over and over and it won't fuck up. but we happen to run it on new transactions, every day, etc
-- Also, I don't think it should actually transfer any money. It feels like that's more formal.

handler
  :: ( Service m Banks
     , Service m AccountStore
     , MonadError EvaluateError m
     )
  => Guid Account -> m ()
handler accountId = do

    account  <- run (AccountStore.Find accountId)
                  >>= require MissingAccount

    checking <- updateBankBalances accountId (private $ bankToken account)
                  >>= require MissingChecking


    -- TODO AccountStore: load approval amount, should always have one
    let advances = []
        info     = AccountHealth.Info (credit account) (balance checking) advances
        health   = AccountHealth.analyze info


    handleHealth health

    pure ()

  where

    require :: MonadError EvaluateError m => (Guid Account -> EvaluateError) -> (Maybe a) -> m a
    require err Nothing = throwError (err accountId)
    require _ (Just a) = pure a




-- | updates the bank accounts and returns the checking account
updateBankBalances
    :: ( Service m Banks
       , Service m AccountStore
       )
    => Guid Account -> Token Access -> m (Maybe BankAccount)
updateBankBalances accountId token = do
    banks <- run $ Bank.LoadAccounts token
    let accounts = List.map (toBankAccount accountId) banks
    run $ AccountStore.SetBankAccounts accountId accounts
    pure $ List.find isChecking accounts
  where
    isChecking :: BankAccount -> Bool
    isChecking acc = accountType acc == Checking



handleHealth :: Monad m => Health -> m ()
handleHealth Ok = pure ()
handleHealth (Maxed _ _) = pure ()
handleHealth (Needs _) = do
    -- TODO store advances, load
    -- TODO schedule advance
    -- TODO schedule payment

    pure ()





data EvaluateError
    = MissingAccount  (Guid Account)
    | MissingChecking (Guid Account)
    deriving (Show, Eq)

instance Exception EvaluateError
