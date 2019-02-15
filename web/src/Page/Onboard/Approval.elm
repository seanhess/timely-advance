module Page.Onboard.Approval exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Encode as Encode
import Platform.Updates exposing (Updates, base, command, set)
import Process
import Route
import Task
import Timely.Api as Api exposing (Account, AccountId, Application, ApprovalResult(..), Id(..))
import Timely.Components as Components exposing (spinnerRipple)
import Timely.Style as Style


type alias Model =
    { key : Nav.Key
    , accountId : Id AccountId
    , status : Status
    }


type alias PublicToken =
    String


type Status
    = Loading
    | Error (List String)
    | Complete ApprovalResult


type alias Problem =
    String


type Msg
    = OnResult (Result Http.Error ApprovalResult)
    | OnWaited ()
    | Close


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key accountId =
    ( { accountId = accountId
      , status = Loading
      , key = key
      }
    , Api.getApplicationResult OnResult accountId
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    let
        updates =
            base model

        isComplete result =
            case result of
                Approved app ->
                    case app.onboarding of
                        Api.Complete ->
                            True

                        Api.Error ->
                            True

                        _ ->
                            False

                _ ->
                    False
    in
    case msg of
        OnResult (Err (Http.BadStatus 404)) ->
            updates
                |> command (Process.sleep 1000 |> Task.perform OnWaited)

        OnResult (Err e) ->
            updates
                |> set { model | status = Error [ Debug.toString e ] }

        OnResult (Ok r) ->
            updates
                |> set { model | status = Complete r }
                |> command
                    (if isComplete r then
                        Cmd.none

                     else
                        Process.sleep 1000 |> Task.perform OnWaited
                    )

        OnWaited () ->
            updates
                |> command (Api.getApplicationResult OnResult model.accountId)

        Close ->
            updates
                |> command (Route.pushUrl model.key (Route.Onboard Route.Landing))


view : Model -> Element Msg
view model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back Close
                , el Style.header (text "Approval")
                ]
            ]
        , column Style.section
            [ viewStatus model.accountId model.status
            ]
        ]


viewStatus : Id AccountId -> Status -> Element Msg
viewStatus accountId status =
    case status of
        Loading ->
            spinnerRipple

        Error ps ->
            Element.column [] (List.map viewProblem ps)

        Complete (Denied d) ->
            Element.el [] (text "Denied")

        Complete (Approved a) ->
            Element.column [ spacing 10, width fill ]
                [ Element.el [] (text "Approved!")
                , Element.el [] (text <| String.fromInt a.approvalAmount)
                , Element.column [ width fill ]
                    [ case a.onboarding of
                        Api.Pending ->
                            Element.el [] (text "Creating your account...")

                        Api.Error ->
                            Element.el [ Style.error ] (text "There was an error!")

                        Api.Complete ->
                            Element.link Style.button
                                { url = Route.url <| Route.Account accountId <| Route.AccountMain
                                , label = Element.text "My Account"
                                }
                    ]
                ]


viewProblem : Problem -> Element Msg
viewProblem p =
    el [] (text p)
