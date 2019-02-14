module Timely.Advances.Credit where


import           Data.Model.Money          (Money)
import           Timely.AccountStore.Types (Account)
import qualified Timely.AccountStore.Types as Accounts (Account (credit))
import           Timely.Advances           (Advance)
import qualified Timely.Advances           as Advances


isEnough :: Money -> Account -> [Advance] -> Bool
isEnough amount account advances =
  let creditUsed = sum (map Advances.amount advances)
      creditRemaining = Accounts.credit account - creditUsed
  in amount <= creditRemaining