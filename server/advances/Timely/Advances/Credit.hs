module Timely.Advances.Credit where


import Timely.AccountStore.Types (Account)
import qualified Timely.AccountStore.Types as Accounts (Account(credit))
import Timely.Advances (Advance)
import qualified Timely.Advances as Advances
import Timely.Types.Money (Money)


isEnough :: Money -> Account -> [Advance] -> Bool
isEnough amount account advances =
  let creditUsed = sum (map Advances.amount advances)
      creditRemaining = Accounts.credit account - creditUsed
  in amount <= creditRemaining
