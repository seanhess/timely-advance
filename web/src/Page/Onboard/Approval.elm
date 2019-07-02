module Page.Onboard.Approval exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Encode as Encode
import Platform.Updates exposing (Updates, command, set, updates)
import Process
import Route
import Task
import Timely.Api as Api exposing (Account, AccountId, Application, ApprovalResult(..), Onboarding(..))
import Timely.Components as Components exposing (spinnerRipple)
import Timely.Resource as Resource exposing (Resource(..))
import Timely.Style as Style
import Timely.Types exposing (Id(..))


type alias Model =
    { key : Nav.Key
    , accountId : Id AccountId
    , application : Resource Application
    , result : Resource ApprovalResult
    }


type alias PublicToken =
    String


type alias Problem =
    String


type Msg
    = OnApplication (Result Http.Error Application)
    | OnWaited ()
    | OnResult (Result Http.Error ApprovalResult)
    | Close


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key accountId =
    ( { accountId = accountId
      , application = Loading
      , result = Loading
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
                Api.getApplicationResult OnResult model.accountId

            else
                Process.sleep 1000 |> Task.perform OnWaited

        isComplete onboarding =
            case onboarding of
                Api.Complete ->
                    True

                Api.Error ->
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

        OnResult rr ->
            updates { model | result = Resource.fromResult rr }

        OnWaited () ->
            updates model
                |> command (Api.getApplication OnApplication model.accountId)

        Close ->
            updates model
                |> command (Route.pushUrl model.key (Route.Onboard Route.Landing))


view : Model -> Element Msg
view model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back Close
                , el Style.heading (text "Approval")
                ]
            ]
        , column Style.section
            [ viewStatus model.accountId model.application model.result
            ]
        ]


viewStatus : Id AccountId -> Resource Application -> Resource ApprovalResult -> Element Msg
viewStatus accountId rapp rres =
    case ( rapp, rres ) of
        ( Failed e, _ ) ->
            viewProblem e

        ( _, Failed e ) ->
            viewProblem e

        ( Loading, _ ) ->
            spinnerRipple

        ( _, Loading ) ->
            spinnerRipple

        ( Ready app, Ready res ) ->
            viewApplication accountId app.onboarding res


viewApplication : Id AccountId -> Onboarding -> ApprovalResult -> Element Msg
viewApplication accountId onboarding res =
    case res of
        Denied _ ->
            Element.el [] (text "Denied")

        Approved a ->
            Element.column [ spacing 10, width fill ]
                [ Element.el [] (text "Approved!")
                , Element.el [] (text <| String.fromInt a.approvalAmount)
                , Element.column [ width fill ]
                    [ case onboarding of
                        Api.Pending ->
                            Element.el [] (text "Creating your account...")

                        Api.Error ->
                            Element.el [ Style.error ] (text "There was an error!")

                        Api.Complete ->
                            Element.link (Style.button Style.primary)
                                { url = Route.url <| Route.Account accountId <| Route.AccountMain
                                , label = Element.text "My Account"
                                }
                    ]
                ]


viewProblem : Http.Error -> Element Msg
viewProblem _ =
    el [] (text "Loading Error")
