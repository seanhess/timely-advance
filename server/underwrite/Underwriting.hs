module Underwriting
  ( Underwriting(..)
  , module Underwriting.Types
  ) where

import Control.Monad.Service (Service(..))

-- I need these common types here!
import AccountStore.Types (Application)
import Underwriting.Types
import qualified Types.Money as Money



data Underwriting a where

    -- we need a lot more information than this
    -- TODO storage? Save everything
    New :: Application -> Underwriting Result


instance Monad m => Service m Underwriting where
    run (New a) = underwrite a


underwrite :: Monad m => Application -> m Result
underwrite _ = pure $ Approved $ Approval $ Money.fromFloat 200.00
