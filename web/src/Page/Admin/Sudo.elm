module Page.Admin.Sudo exposing (Model, Msg(..), init, label, subscriptions, update, view)

-- import Debug

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Html
import Http
import Json.Encode as Encode
import Platform.Updates exposing (Updates, command, set, updates)
import Route
import Timely.Api as Api exposing (AccountInfo, Application, Auth, AuthCode, Bank, Id(..), Phone, Session, Token, idValue)
import Timely.Components as Components exposing (loadingButton)
import Timely.Style as Style


type alias Model =
    { secret : String
    , key : Nav.Key
    , session : Status Session
    }


type Status a
    = None
    | Loading
    | Failed Http.Error
    | Ready a


type Msg
    = EditSecret String
    | Submit
    | OnSession (Result Http.Error Session)
    | OnLogin (Result Http.Error Session)
    | Logout
    | OnLogout (Result Http.Error ())


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { secret = ""
      , key = key
      , session = Loading
      }
    , Api.sessionsGet OnSession
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    -- Debug.log (Debug.toString msg) <|
    case msg of
        EditSecret s ->
            updates { model | secret = s }

        Submit ->
            updates model
                |> command (Api.sessionsAuthAdmin OnLogin model.secret)

        OnLogin (Err e) ->
            updates { model | session = Failed e }

        OnLogin (Ok session) ->
            updates { model | session = Ready session }

        OnSession (Err _) ->
            updates { model | session = None }

        OnSession (Ok session) ->
            updates { model | session = Ready session }

        Logout ->
            updates model |> command (Api.sessionsLogout OnLogout)

        OnLogout _ ->
            updates { model | session = None }


view : Model -> Element Msg
view model =
    column Style.formPage <|
        case model.session of
            Ready s ->
                [ viewSuperuser model s ]

            Loading ->
                [ Components.spinner ]

            Failed e ->
                [ viewLogin model, viewProblems e ]

            None ->
                [ viewLogin model ]


viewLogin : Model -> Element Msg
viewLogin model =
    column [ spacing 36, width fill ]
        [ el Style.header (text "Sudo")
        , Input.text []
            { text = model.secret
            , placeholder = Nothing
            , onChange = EditSecret
            , label = label "Passphrase"
            }
        , Input.button (Style.button Style.primary)
            { onPress = Just Submit
            , label = Element.text "Go"
            }
        ]


viewSuperuser : Model -> Session -> Element Msg
viewSuperuser model session =
    column [ spacing 20 ]
        [ el Style.header (text "Welcome! ")
        , el [] (text "You are as handsome as you are intelligent")
        , Input.button [ Style.link ] { onPress = Just Logout, label = text "Logout" }
        ]


label : String -> Input.Label Msg
label t =
    Input.labelAbove [ Font.size 14 ] (text t)


viewProblems : Http.Error -> Element Msg
viewProblems _ =
    el [ Style.error ] (text "Nope")
