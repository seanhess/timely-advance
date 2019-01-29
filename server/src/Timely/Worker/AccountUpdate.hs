{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
module Timely.Worker.AccountUpdate where

import           Control.Exception             (Exception)
import           Control.Monad.Except          (MonadError (..))
import           Control.Monad.Service         (Service (run))
import qualified Data.List                     as List
import qualified Network.AMQP.Worker           as Worker

import           Timely.AccountStore.Account   (AccountStore)
import qualified Timely.AccountStore.Account   as AccountStore
import           Timely.AccountStore.Types     (Account (bankToken, credit), BankAccount (accountType, balance),
                                                BankAccountType (Checking), toBankAccount)
import           Timely.Advances               (Advances)
import qualified Timely.Advances               as Advances
import           Timely.Bank                   (Access, Banks, Token)
import qualified Timely.Bank                   as Bank
import           Timely.Evaluate.AccountHealth (Health (..))
import qualified Timely.Evaluate.AccountHealth as AccountHealth
import           Timely.Events                 as Events
import           Timely.Types.Guid             (Guid)
import           Timely.Types.Private          (Private (..))


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
     , Service m Advances
     , MonadError EvaluateError m
     )
  => Guid Account -> m ()
handler accountId = do

    account  <- run (AccountStore.Find accountId)
                  >>= require MissingAccount

    checking <- updateBankBalances accountId (private $ bankToken account)
                  >>= require MissingChecking

    advances <- run (Advances.FindActive accountId)

    let health   = AccountHealth.analyze $ AccountHealth.Info (credit account) (balance checking) advances

    handleHealth health

  where

    require :: MonadError EvaluateError m => (Guid Account -> EvaluateError) -> (Maybe a) -> m a
    require err Nothing = throwError (err accountId)
    require _ (Just a)  = pure a




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
    -- TODO store advances
    -- TODO schedule advance
    -- TODO schedule payment

    pure ()





data EvaluateError
    = MissingAccount  (Guid Account)
    | MissingChecking (Guid Account)
    deriving (Show, Eq)

instance Exception EvaluateError
