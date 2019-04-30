module Timely.Api.Admin where


import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Selda          (Selda)
import           Data.Model.Guid              (Guid)
import qualified Timely.Accounts.Account      as Accounts
import qualified Timely.Accounts.Budgets      as Budgets
import qualified Timely.Accounts.Transactions as Transactions
import           Timely.Accounts.Types        (Account)



-- It needs to delete stuff all over the place
deleteAccount
  :: (MonadIO m, Selda m)
  => Guid Account -> m ()
deleteAccount i = do
  Transactions.deleteAccount i
  Budgets.deleteAccount i
  Accounts.deleteAccount i
  pure ()
