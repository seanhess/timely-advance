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
import Timely.Api as Api exposing (AccountId, Application, ApprovalResult(..))
import Timely.Style as Style


type alias Model =
    { key : Nav.Key
    , accountId : AccountId
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


init : Nav.Key -> AccountId -> ( Model, Cmd Msg )
init key accountId =
    ( { accountId = accountId
      , status = Loading
      , key = key
      }
    , Api.getApplicationResultById OnResult accountId
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    let
        updates =
            base model
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

        OnWaited () ->
            updates
                |> command (Api.getApplicationResultById OnResult model.accountId)


view : Model -> Element Msg
view model =
    Element.column Style.formPage
        [ el Style.header (text "Approval")
        , viewStatus model.status
        ]


viewStatus : Status -> Element Msg
viewStatus status =
    case status of
        Loading ->
            Element.el [] (text "Loading...")

        Error ps ->
            Element.column [] (List.map viewProblem ps)

        Complete (Denied d) ->
            Element.el [] (text "Denied")

        Complete (Approved a) ->
            Element.column []
                [ Element.el [] (text "Approved!")
                , Element.el [] (text <| String.fromInt a.approvalAmount)
                ]


viewProblem : Problem -> Element Msg
viewProblem p =
    el [] (text p)
