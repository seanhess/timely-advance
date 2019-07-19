module Page.Admin.Test exposing (Model, Msg(..), init, update, view)

-- Client -> Server Integration tests

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Region as Region
import Http exposing (Error(..))
import Page.Onboard.Approval as Approval
import Platform.Updates exposing (Updates, command, set, updates)
import Process
import Route exposing (Onboard(..), Route(..))
import Task
import Timely.Api as Api exposing (Customer)
import Timely.Icons as Icons exposing (Size(..))
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types exposing (Id(..), Valid(..))
import Timely.Types.Account exposing (AccountId)
import Timely.Types.AccountHealth exposing (AccountHealth)
import Timely.Types.Application exposing (Application, Onboarding(..))
import Timely.Types.Date exposing (formatDate)
import Timely.Types.Money exposing (formatMoney)


type alias Model =
    { step : Step
    , email : String
    , history : List Step
    }


init : ( Model, Cmd Msg )
init =
    ( { step = Init
      , email = "test@timelyadvance.com"
      , history = []
      }
    , Cmd.none
    )



-- Ok what does "Step" mean. It's the current state of the thing
-- I think this makes sense


type Step
    = Passed
    | Aborted Http.Error
    | Init
    | Application Application
    | Account (Id AccountId) AccountHealth


type Msg
    = Step Step
    | OnApplication (Result Http.Error Application)
    | OnAccountHealth (Id AccountId) (Result Http.Error AccountHealth)
    | Run (Cmd Msg) ()
    | Delete (Id AccountId)
    | OnDeleted (Result Http.Error String)


nextStep : Model -> Step -> Cmd Msg
nextStep model step =
    case step of
        Aborted _ ->
            Cmd.none

        Passed ->
            Cmd.none

        Init ->
            Api.postApplications OnApplication { email = model.email, publicBankToken = Id "test-bank-token" }

        Application app ->
            case app.onboarding of
                Pending _ ->
                    Approval.poll (Run <| Api.getApplication OnApplication app.accountId)

                Complete ->
                    -- also, there's a bunch of stuff
                    -- not easy to tie together into one thing
                    -- but we should do ONE request at a time
                    Api.getAccountHealth (OnAccountHealth app.accountId) app.accountId

                _ ->
                    Cmd.none

        Account _ _ ->
            Cmd.none


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update key msg model =
    let
        onStep r next =
            case r of
                Err e ->
                    updates model

                Ok a ->
                    next a

        runNext s =
            updates { model | step = s, history = model.history ++ [ s ] }
                |> command (nextStep model s)
    in
    case msg of
        Step s ->
            runNext s

        OnApplication r ->
            onStep r <| \app -> runNext (Application app)

        OnAccountHealth id r ->
            onStep r <| \health -> runNext (Account id health)

        Run cmd _ ->
            updates model |> command cmd

        Delete accountId ->
            updates model |> command (Api.deleteAccount OnDeleted accountId)

        OnDeleted _ ->
            updates { model | history = [] }


view : Model -> Element Msg
view model =
    column Style.section
        [ link [] { url = Route.url (Route.Admin Route.Sudo), label = Icons.icon Icons.back Icons.Big }
        , el Style.heading (text "TESTS")
        , button (Style.button Style.primary) { onPress = Just (Step Init), label = text "Run Tests" }
        , column [ Border.widthXY 0 1, spacing 10, padding 10, width fill ] (List.map viewStep model.history)
        ]


viewStep : Step -> Element Msg
viewStep step =
    case step of
        Init ->
            text "Init"

        Aborted _ ->
            text "Error"

        Passed ->
            text "Passed"

        Application app ->
            row [ spacing 5 ]
                [ text "Application"
                , text "-"
                , case app.onboarding of
                    Complete ->
                        text "complete"

                    Pending p ->
                        Approval.viewPending p

                    Rejected r ->
                        text "Rejected"

                    Error ->
                        row [ spacing 10 ]
                            [ text "Error, check logs" ]
                ]

        Account id health ->
            link [ Style.link ]
                { url = Route.url <| Route.Admin <| Route.Customer id, label = text "Account" }


formatError : Http.Error -> String
formatError e =
    case e of
        BadStatus i ->
            "BadStatus: " ++ String.fromInt i

        BadBody s ->
            "BadBody: " ++ s

        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        BadUrl s ->
            "BadUrl: " ++ s
