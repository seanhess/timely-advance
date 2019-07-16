module Page.Admin.Test exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Region as Region
import Http
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
import Timely.Types.Application exposing (Application, Onboarding(..))
import Timely.Types.Date exposing (formatDate)


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



-- it already has all the historical transactions, so you can just go for it
-- but you have to let the system know it's ready
-- so send the webhook?
-- yeah we're going to need to be able to send webhooks here


type Step
    = Passed
    | Aborted Http.Error
    | Init
    | Application Application
    | Approved Application


type Msg
    = Step Step
    | OnApplication (Result Http.Error Application)
    | OnWaited (Cmd Msg) ()


runStep : Model -> Step -> Cmd Msg
runStep model step =
    case step of
        Aborted _ ->
            Cmd.none

        Passed ->
            Cmd.none

        Init ->
            -- you have to think about the next step here
            Api.postApplications OnApplication { email = model.email, publicBankToken = Id "test-bank-token" }

        Application app ->
            case app.onboarding of
                Pending _ ->
                    Approval.poll (OnWaited <| Api.getApplication OnApplication app.accountId)

                _ ->
                    Cmd.none

        Approved _ ->
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

        step s =
            updates { model | step = s, history = model.history ++ [ s ] }
                |> command (runStep model s)
    in
    case msg of
        Step s ->
            step s

        OnApplication r ->
            onStep r <| \app -> step (Application app)

        OnWaited cmd _ ->
            updates model |> command cmd


view : Model -> Element Msg
view model =
    column Style.section
        [ link [] { url = Route.url (Route.Admin Route.Sudo), label = Icons.icon Icons.back Icons.Big }
        , el Style.heading (text "TESTS")
        , button (Style.button Style.primary) { onPress = Just (Step Init), label = text "Run Tests" }
        , column [ Border.widthXY 0 1, spacing 10 ] (List.map viewStep model.history)
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
                        text "Error"
                ]

        Approved app ->
            text "Approved"



-- Webhooks ------------------------------------
-- type BankItem
--     = BankItemEmpty
-- -- but then you need the item id
-- -- I don't know the webook :(
-- type alias PlaidWebhook =
--     { item_id : Id BankItem
--     , webhook_code : TransactionsCode
--     , new_transactions : Int
--     }
-- type TransactionsCode
--     = INITIAL_UPDATE
--     | HISTORICAL_UPDATE
--     | DEFAULT_UPDATE
-- postWebhook : (Result Http.Error String -> msg) -> PlaidWebhook -> Cmd msg
-- postWebhook toMsg body =
--     Api.requestPOST toMsg [ "", "v1", "webhooks", "plaid" ] (encodeWebhook body) Application.decode
-- -- we need to look them up by item id, unfortunately
