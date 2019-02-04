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
import Timely.Api as Api exposing (Account, Advance, Application, ApprovalResult(..), Id(..), Money(..), idValue)
import Timely.Components exposing (spinnerRipple)
import Timely.Style as Style


type alias Model =
    { key : Nav.Key
    , accountId : Id Account
    , advanceId : Id Advance
    , acceptAmount : Money
    , status : Status
    }


type Status
    = Loading
    | Error (List String)
    | Loaded Advance
    | Accepted


type Msg
    = OnResult (Result Http.Error Advance)
    | Edit String
    | Submit
    | OnAccept (Result Http.Error ())


init : Nav.Key -> Id Account -> Id Advance -> ( Model, Cmd Msg )
init key accountId advanceId =
    ( { accountId = accountId
      , advanceId = advanceId
      , status = Loading
      , key = key
      , acceptAmount = Money 0
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
                |> set { model | status = Loaded a, acceptAmount = a.offer }

        Edit s ->
            let
                amount =
                    Maybe.withDefault 0 (String.toInt s)
            in
            updates |> set { model | acceptAmount = Money amount }

        Submit ->
            updates
                |> command (Api.postAdvanceAccept OnAccept model.accountId model.advanceId model.acceptAmount)

        OnAccept (Err e) ->
            updates
                |> set { model | status = Error [ Debug.toString e ] }

        OnAccept (Ok _) ->
            -- TODO what should we do when they accept?
            updates |> set { model | status = Accepted }


view : Model -> Element Msg
view model =
    Element.column Style.formPage
        [ el Style.header (text "Advance")
        , el [] (text <| idValue model.accountId)
        , el [] (text <| idValue model.advanceId)
        , viewStatus model model.status
        ]


viewForm : Model -> Element Msg
viewForm model =
    let
        (Money v) =
            model.acceptAmount
    in
    Element.column [ spacing 15 ]
        [ Input.text []
            { text = String.fromInt v
            , placeholder = Nothing
            , onChange = Edit
            , label = Input.labelAbove [ Font.size 14 ] (text "Amount")
            }
        , Input.button Style.button
            { onPress = Just Submit
            , label = Element.text "Accept"
            }
        ]


viewStatus : Model -> Status -> Element Msg
viewStatus model status =
    case status of
        Loading ->
            spinnerRipple

        Error ps ->
            Element.column [] (List.map viewProblem ps)

        Loaded a ->
            viewForm model

        Accepted ->
            Element.el [] (text "Yay! Your money is on its way")


viewProblem : String -> Element Msg
viewProblem p =
    el [] (text p)
