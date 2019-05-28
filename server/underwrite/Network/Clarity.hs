{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Clarity
  ( Request(..)
  , Frequency(..)
  , InquiryPurposeType(..)
  , BankAccountType(..)
  , InquiryTradelineType(..)
  , RoutingNumber
  , AccountNumber
  , GenerationCode
  , Employer(..)
  , Valid
  , validate
  , Address(..)
  , document
  -- * Response Parsers
  , inquiry
  , bankBehavior
  , creditRisk
  , advancedAttributes
  , fraud
  , fraudInsight
  ) where


-- https://login.clarityservices.com/interactive_xmls/inquiry
-- https://login.clarityservices.com/interactive_xmls/inquiry_response

import Network.Clarity.Request
import Network.Clarity.Response
import Data.Model.Valid (Valid, validate)
import Data.Model.Types (Address(..))
