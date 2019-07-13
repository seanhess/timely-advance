module Page.Onboard.Approval exposing (Model, Msg, init, subscriptions, update, view)

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
    { key : Nav.Key
    , accountId : Id AccountId
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


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key accountId =
    ( { accountId = accountId
      , application = Loading
      , key = key
      }
    , Api.getApplication OnApplication accountId
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    let
        pollApplication app =
            if isComplete app.onboarding then
                Cmd.none

            else
                Process.sleep 1000 |> Task.perform OnWaited

        isComplete onboarding =
            case onboarding of
                Complete ->
                    True

                Error ->
                    True

                Rejected _ ->
                    True

                _ ->
                    False
    in
    case msg of
        -- OnApplication (Err (Http.BadStatus 404)) ->
        --     updates
        --         |> command (Process.sleep 1000 |> Task.perform OnWaited)
        OnApplication (Err e) ->
            updates { model | application = Failed e }

        OnApplication (Ok app) ->
            updates { model | application = Ready app }
                |> command (pollApplication app)

        OnWaited () ->
            updates model
                |> command (Api.getApplication OnApplication model.accountId)

        Back ->
            updates model
                |> command (Route.pushUrl model.key (Route.Onboard Route.Landing))


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


viewPending : Pending -> Element Msg
viewPending p =
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
