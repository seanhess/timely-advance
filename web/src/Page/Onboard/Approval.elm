module Page.Onboard.Approval exposing (Model, Msg, checkApplication, init, poll, subscriptions, update, view, viewPending)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Http
import Json.Encode as Encode
import Platform.Updates exposing (Updates, command, set, updates)
import Process
import Route
import Task
import Timely.Api as Api exposing (ApprovalResult(..))
import Timely.Components as Components exposing (spinnerRipple)
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types exposing (Id(..))
import Timely.Types.Account exposing (AccountId)
import Timely.Types.Application exposing (Application, Onboarding(..), Pending(..), Rejected(..))


type alias Model =
    { accountId : Id AccountId
    , application : Resource Application
    }


type alias PublicToken =
    String


type alias Problem =
    String


type Msg
    = OnApplication (Result Http.Error Application)
    | OnWaited ()
    | Back


init : Id AccountId -> ( Model, Cmd Msg )
init accountId =
    ( { accountId = accountId
      , application = Loading
      }
    , Api.getApplication OnApplication accountId
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


poll : (() -> msg) -> Cmd msg
poll onWaited =
    Process.sleep 1000 |> Task.perform onWaited


checkApplication : (() -> msg) -> Cmd msg -> Application -> Cmd msg
checkApplication onWaited complete app =
    case app.onboarding of
        Complete ->
            complete

        Error ->
            Cmd.none

        Rejected _ ->
            Cmd.none

        _ ->
            poll onWaited


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update key msg model =
    case msg of
        OnApplication (Err e) ->
            updates { model | application = Failed e }

        OnApplication (Ok app) ->
            updates { model | application = Ready app }
                |> command (checkApplication OnWaited (Route.goAccount key app.accountId) app)

        OnWaited () ->
            updates model
                |> command (Api.getApplication OnApplication model.accountId)

        Back ->
            updates model
                |> command (Route.pushUrl key (Route.Onboard Route.Landing))


view : Model -> Element Msg
view model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back Back
                , el Style.heading (text "Approval")
                ]
            ]
        , column Style.section
            [ resource (viewStatus model.accountId) model.application
            ]
        ]


viewStatus : Id AccountId -> Application -> Element Msg
viewStatus accountId app =
    viewApplication accountId app.onboarding


viewApplication : Id AccountId -> Onboarding -> Element Msg
viewApplication accountId onboarding =
    Element.column [ spacing 10, width fill ]
        [ Element.column [ width fill ]
            [ case onboarding of
                Error ->
                    Element.el [ Style.error ] (text "There was an error!")

                Pending p ->
                    viewPending p

                Rejected r ->
                    viewRejected r

                Complete ->
                    Element.link (Style.button Style.primary)
                        { url = Route.url <| Route.Account accountId <| Route.AccountMain
                        , label = Element.text "My Account"
                        }
            ]
        ]


viewPending : Pending -> Element msg
viewPending p =
    row [ spacing 20, width fill ]
        [ Components.spinnerRipple
        , el [ padding 10 ] <|
            case p of
                New ->
                    text "Initializing"

                Bank ->
                    text "Loading your bank details"

                Transfers ->
                    text "Loading your transfer account"

                Transactions ->
                    text "Analyzing your transactions"

                Creation ->
                    text "Creating your Account"
        ]


viewRejected : Rejected -> Element Msg
viewRejected r =
    let
        item msg isNope =
            row [ width fill, spacing 10 ]
                [ paragraph [ width fill ] [ text msg ]
                , el [ alignRight ]
                    (if isNope then
                        nope

                     else
                        check
                    )
                ]

        check =
            el [] (text "√")

        nope =
            el [ Font.color Style.red ] (text "X")
    in
    column [ spacing 15, width fill ]
        [ paragraph Style.heading [ text "We can't set up your account at this time" ]
        , paragraph [] [ text "You must meet all the following requirements" ]
        , el [ Border.widthXY 0 1, width fill, height (px 1) ] (text "")

        -- they can't get this far without having a bank account
        , item "Active account at a supported bank" False
        , item "Reliable source of income" (r == IncomeNotRegular)
        , item "Income that exceeds recurring bills" (r == IncomeLow)
        , button (Style.button Style.primary) { onPress = Just Back, label = text "Go Back" }
        ]


viewProblem : Http.Error -> Element Msg
viewProblem _ =
    el [] (text "Loading Error")
