module Network.Clarity.Account where

import Data.Text (Text)

data Account = Account
    { groupId              :: Int
    , accountId            :: Int
    , locationId           :: Int
    , username             :: Text
    , password             :: Text
    , controlFileName      :: Text
    , inquiryPurposeType   :: InquiryPurposeType
    , inquiryTradelineType :: InquiryTradelineType
    }


data InquiryPurposeType
  = AR -- ^ New Credit
  | AS -- ^ New Credit Soft
  | RA -- ^ Account Review Soft
  | RP -- ^ Consumer Inquiry Soft
  | CL -- ^ Collection Inquiry
  | PC -- ^ Pre-check Soft
  | MS -- ^ Credit Monitor Soft
  | CC -- ^ Check Cash
  | CS -- ^ Collection Soft
  | PS -- ^ Pre-screen Soft
  | IV -- ^ Item Verification
  | IS -- ^ Item Verification Soft
  | EH -- ^ Employment
  | ES -- ^ Employment Soft
  | LH -- ^ Lease
  | LS -- ^ Lease Soft
  | WS -- ^ Written Authorization Soft
  deriving (Show)


-- Tons of options, see docs
newtype InquiryTradelineType = InquiryTradelineType { itt :: Text }
