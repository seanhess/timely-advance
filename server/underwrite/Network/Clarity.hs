{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Clarity
  ( Account(..)
  , Consumer(..)
  , Frequency(..)
  , InquiryPurposeType(..)
  , BankAccountType(..)
  , InquiryTradelineType(..)
  , RoutingNumber
  , AccountNumber
  , GenerationCode
  , Employer(Employer)
  , Valid
  , validate
  , Address(Address)
  , document
  , inquiry
  ) where


-- https://login.clarityservices.com/interactive_xmls/inquiry
-- https://login.clarityservices.com/interactive_xmls/inquiry_response

import Network.Clarity.Request
import Network.Clarity.Consumer
import Network.Clarity.Employer
import Network.Clarity.Account
import Data.Model.Valid (Valid, validate)
import Data.Model.Types (Address(..))
