port module Page.Onboard.Bank exposing (Model, Msg(..), init, label, plaidLinkDone, plaidLinkExit, plaidLinkOpen, subscriptions, update, view, viewPlaidLanding, viewProblems)

import Browser.Navigation as Nav
import Date
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Html
import Http
import Iso8601
import Json.Encode as Encode
import Platform.Updates exposing (Updates, command, set, updates)
import Route exposing (Route)
import Task
import Time
import Timely.Api as Api exposing (Session)
import Timely.Components as Components exposing (loadingButton)
import Timely.Resource as Resource exposing (Resource(..))
import Timely.Style as Style
import Timely.Types exposing (Id(..), Phone, Token, Valid(..), idValue)
import Timely.Types.Application exposing (AccountInfo, Application, Bank)
import Timely.Types.Date as Date exposing (Date)


port plaidLinkOpen : Encode.Value -> Cmd msg


port plaidLinkExit : (Encode.Value -> msg) -> Sub msg


port plaidLinkDone : (String -> msg) -> Sub msg


type alias Model =
    { now : Date
    , email : String
    , plaidToken : Token Bank
    , application : Resource Application
    }


type Msg
    = Back
    | OnDate Date
    | PlaidOpen
    | OnPlaidExited
    | OnPlaidDone String
    | OnSignup (Result Http.Error Application)


init : String -> ( Model, Cmd Msg )
init email =
    ( { now = Date.empty
      , email = email
      , plaidToken = Id ""
      , application = Loading
      }
    , Cmd.batch
        [ Date.current OnDate
        , plaidLinkOpen Encode.null
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ plaidLinkExit (always OnPlaidExited)
        , plaidLinkDone OnPlaidDone
        ]


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update key msg model =
    let
        newApplication token =
            { email = model.email
            , publicBankToken = token
            }
    in
    case msg of
        Back ->
            updates model
                |> command (Route.pushUrl key <| Route.Onboard Route.Signup)

        OnDate t ->
            updates { model | now = t }

        PlaidOpen ->
            updates model
                |> command (plaidLinkOpen Encode.null)

        OnPlaidExited ->
            -- Leave this here. Elm throws an error if there are no subscribers for a subscription!
            updates model

        OnPlaidDone token ->
            -- it hasn't saved yet
            updates { model | plaidToken = Id token, application = Loading }
                |> command (Api.postApplications OnSignup <| newApplication (Id token))

        OnSignup (Err e) ->
            updates { model | application = Failed e }

        OnSignup (Ok a) ->
            updates { model | application = Ready a }
                |> command (Nav.pushUrl key (Route.url <| Route.Onboard <| Route.Approval <| a.accountId))


view : Model -> Element Msg
view model =
    viewPlaidLanding model


viewPlaidLanding : Model -> Element Msg
viewPlaidLanding model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back Back
                , paragraph [] [ text "Connect your bank" ]
                ]
            ]
        , column Style.section
            [ Input.button (Style.button Style.primary)
                { onPress = Just PlaidOpen
                , label = Style.label "Connect Bank"
                }
            , viewProblems model.application
            ]
        ]


label : String -> Input.Label Msg
label t =
    Input.labelAbove [ Font.size 14 ] (text t)


viewProblems : Resource a -> Element Msg
viewProblems resource =
    case resource of
        Failed e ->
            el [ Style.error ] (text "Application error")

        _ ->
            Element.none
