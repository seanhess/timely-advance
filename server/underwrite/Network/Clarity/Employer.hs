module Network.Clarity.Employer where

import Data.Text (Text)
import Data.Model.Types (Valid, State)


-- prefix employer
data Employer = Employer
    { name    :: Text
    , address :: Maybe Text
    , city    :: Maybe Text
    , state   :: Maybe (Valid State)
    }

