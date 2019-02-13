module Timely.Transfers.Account where


import           Network.Dwolla            (FundingSource)


-- | These are secretly FundingSource Ids, so we can just use them instead of looking them up
type TransferAccount = FundingSource
