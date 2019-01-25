module Timely.Worker.Evaluate where

import           Control.Monad.Service (Service(run))
import           Control.Exception (Exception, throw)
import qualified Data.List as List
import           Data.Function ((&))
import qualified Network.AMQP.Worker as Worker

import qualified Timely.Bank as Bank
import           Timely.Bank (Banks)
import qualified Timely.Evaluate.AccountHealth as AccountHealth
import           Timely.Evaluate.AccountHealth (Health(..))
import qualified Timely.AccountStore.Account as AccountStore
import           Timely.AccountStore.Account (AccountStore)
import           Timely.AccountStore.Types (Account(bankToken), BankAccount(accountType, balance), BankAccountType(Checking), toBankAccount)
import           Timely.Types.Guid (Guid)
import           Timely.Types.Private (Private(..))
import qualified Timely.Types.Money as Money
import           Timely.Events as Events


queue :: Worker.Queue (Guid Account)
queue = Worker.topic Events.transactionsNew "app.evaluate"


handler
  :: ( Service m Banks
     , Service m AccountStore
     )
  => Guid Account -> m ()
handler accountId = do

    ma <- run $ AccountStore.Find accountId
    -- what if we can't find the account in question?
    -- definitely throw an error
    let account = require MissingAccount ma

    -- what if we can't find the checking account?
    -- throw another error!
    -- TODO accounts always have a checking account
    banks <- run $ Bank.LoadAccounts $ private $ bankToken account

    let checking = List.map (toBankAccount accountId) banks
                     & List.find isChecking
                     & require MissingChecking

    -- these aren't the same type.. need to update the account's banks first
    -- let checking = require MissingChecking $ 

    -- TODO AccountStore: load approval amount
    let approval = Money.fromFloat 250.00
        advances = []
        info     = AccountHealth.Info approval (balance checking) advances
        health   = AccountHealth.analyze info


    handleHealth health

    pure ()

  where
    require :: (Guid Account -> EvaluateError) -> (Maybe a) -> a
    require err Nothing = throw (err accountId)
    require _ (Just a) = a

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
    = MissingAccount (Guid Account)
    | MissingChecking (Guid Account)
    deriving (Show, Eq)

instance Exception EvaluateError
