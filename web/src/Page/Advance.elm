module Page.Advance exposing (Model, Msg, init, subscriptions, update, view)

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
import Timely.Api as Api exposing (Account, Advance, Application, ApprovalResult(..), Id(..), idValue)
import Timely.Components exposing (spinnerRipple)
import Timely.Style as Style


type alias Model =
    { key : Nav.Key
    , accountId : Id Account
    , advanceId : Id Advance
    , status : Status
    }


type Status
    = Loading
    | Error (List String)
    | Complete Advance


type Msg
    = OnResult (Result Http.Error Advance)


init : Nav.Key -> Id Account -> Id Advance -> ( Model, Cmd Msg )
init key accountId advanceId =
    ( { accountId = accountId
      , advanceId = advanceId
      , status = Loading
      , key = key
      }
    , Api.getAdvance OnResult accountId advanceId
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
        OnResult (Err e) ->
            updates
                |> set { model | status = Error [ Debug.toString e ] }

        OnResult (Ok a) ->
            updates
                |> set { model | status = Complete a }


view : Model -> Element Msg
view model =
    Element.column Style.formPage
        [ el Style.header (text "Advance")
        , el [] (text <| idValue model.accountId)
        , el [] (text <| idValue model.advanceId)
        , viewStatus model.status
        ]


viewStatus : Status -> Element Msg
viewStatus status =
    case status of
        Loading ->
            spinnerRipple

        Error ps ->
            Element.column [] (List.map viewProblem ps)

        Complete a ->
            Element.el [] (text <| Debug.toString a)


viewProblem : String -> Element Msg
viewProblem p =
    el [] (text p)
