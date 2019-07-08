module Timely.Types.Account exposing (Account, AccountId(..))

import Timely.Types exposing (Id(..), decodeId, encodeId)


type AccountId
    = AccountId


type alias Account =
    { accountId : Id AccountId
    , phone : String
    }
