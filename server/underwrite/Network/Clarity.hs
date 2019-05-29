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
  ) where


-- https://login.clarityservices.com/interactive_xmls/inquiry
-- https://login.clarityservices.com/interactive_xmls/inquiry_response

import Network.Clarity.Request
import Data.Model.Valid (Valid, validate)
import Data.Model.Types (Address(..))
