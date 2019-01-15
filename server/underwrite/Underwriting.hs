module Underwriting
  ( Underwriting(..)
  , module Underwriting.Types
  ) where

import Control.Monad.Service (Service(..))

-- I need these common types here!
import AccountStore.Types (Customer(phone))
import Underwriting.Types
import qualified Types.Money as Money



data Underwriting a where

    -- we need a lot more information than this
    -- TODO storage? Save everything
    New :: Customer -> Underwriting Result


instance Monad m => Service m Underwriting where
    run (New a) = underwrite a


underwrite :: Monad m => Customer -> m Result
underwrite app = pure $ mock $ phone app
  where
    mock "100" = Approved $ Approval $ Money.fromFloat 100.00
    mock "200" = Approved $ Approval $ Money.fromFloat 200.00
    mock "300" = Approved $ Approval $ Money.fromFloat 300.00
    mock "400" = Approved $ Approval $ Money.fromFloat 400.00
    mock "denied" = Denied $ Denial NoReason
    mock _ = Denied $ Denial NoReason
