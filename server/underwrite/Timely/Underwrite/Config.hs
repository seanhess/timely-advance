module Timely.Underwite.Config where
import Network.Clarity.Config as Clarity (Config(..))


-- default (test) clarity settings
clarity :: Clarity.Config
clarity = Config
  { groupId  = 101
  , accountId = 201
  , locationId = 8642
  , username             = "timelyadvancetestutility"
  , password             = "Cbuckethead1!"
  , controlFileName      = "Test_TimelyAdvances"
  , inquiryPurposeType   = AR
  , inquiryTradelineType = "C7"
  }
