module Timely.Types.Application exposing (AccountInfo, Application, Bank(..), Onboarding(..), Pending(..), Rejected(..), SSN, decode, decodeOnboarding, encodeAccountInfo, toOnboarding)

import Json.Decode as Decode exposing (Decoder, nullable, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (Value)
import Timely.Types exposing (Id(..), Valid, decodeId, encodeId, encodeValid)
import Timely.Types.Account exposing (AccountId)
import Timely.Types.Date exposing (Date, decodeDate)


type alias Application =
    { accountId : Id AccountId
    , onboarding : Onboarding
    }


type Onboarding
    = Pending Pending
    | Rejected Rejected
    | Error
    | Complete


type Pending
    = New
    | Bank
    | Transfers
    | Transactions
    | Creation


type Rejected
    = IncomeLow
    | IncomeNotRegular


type alias AccountInfo =
    { email : String
    , ssn : Valid SSN
    , dateOfBirth : String
    , publicBankToken : Id Bank
    }


type SSN
    = SSN


type Bank
    = BankEmpty


toOnboarding : String -> Onboarding
toOnboarding s =
    case s of
        "Pending New" ->
            Pending New

        "Pending Bank" ->
            Pending Bank

        "Pending Transfers" ->
            Pending Transfers

        "Pending Transactions" ->
            Pending Transactions

        "Pending Creation" ->
            Pending Creation

        "Rejected IncomeLow" ->
            Rejected IncomeLow

        "Rejected IncomeNotRegular" ->
            Rejected IncomeNotRegular

        "Error" ->
            Error

        "Complete" ->
            Complete

        _ ->
            Error


encodeAccountInfo x =
    Encode.object
        [ ( "email", Encode.string x.email )
        , ( "ssn", encodeValid x.ssn )
        , ( "dateOfBirth", Encode.string x.dateOfBirth )
        , ( "publicBankToken", encodeId x.publicBankToken )
        ]


decode : Decoder Application
decode =
    Decode.succeed Application
        |> required "accountId" decodeId
        |> required "onboarding" decodeOnboarding


decodeOnboarding : Decoder Onboarding
decodeOnboarding =
    Decode.map toOnboarding string
