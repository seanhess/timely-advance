{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Network.Clarity.Response where

-- https://login.clarityservices.com/interactive_xmls/inquiry
-- https://login.clarityservices.com/interactive_xmls/inquiry_response

import Text.XML.Parse          (Parser, element)



-- * Response Parsers


inquiry :: Parser a -> Parser a
inquiry = element "inquiry"


bankBehavior :: Parser a -> Parser a
bankBehavior = element "clear-bank-behavior"


creditRisk :: Parser a -> Parser a
creditRisk = element "clear-credit-risk"


advancedAttributes :: Parser a -> Parser a
advancedAttributes = element "clear-advanced-attributes"


fraud :: Parser a -> Parser a
fraud = element "clear-fraud"


fraudInsight :: Parser a -> Parser a
fraudInsight = element "clear-fraud-insight"



