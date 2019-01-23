port module Page.Onboard.Signup exposing (Mode(..), Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Debug
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Html
import Http
import Json.Encode as Encode
import Platform.Updates exposing (Updates, command, set, updates)
import Route
import Timely.Api as Api exposing (AccountInfo, Application, Auth, AuthCode, Bank, Id(..), Phone, Session, Token, idValue)
import Timely.Components exposing (loadingButton)
import Timely.Style as Style


port plaidLinkOpen : Encode.Value -> Cmd msg


port plaidLinkExit : (Encode.Value -> msg) -> Sub msg


port plaidLinkDone : (String -> msg) -> Sub msg


type alias Model =
    { email : String
    , phone : Phone
    , code : Token AuthCode
    , plaidToken : Token Bank
    , key : Nav.Key
    , step : Step
    , status : Status
    , mode : Mode
    }


type Mode
    = Signup
    | Login


type Step
    = EditingForm
    | EditingCode
    | Plaid


type Status
    = Loading
    | Ready
    | Failed String Http.Error


type Msg
    = EditPhone String
    | EditEmail String
    | SubmitForm
    | EditCode String
    | SubmitCode
    | PlaidOpen
    | PlaidExited
    | PlaidDone String
    | CompletedCheckCode (Result Http.Error Session)
    | CompletedCreateCode (Result Http.Error ())
    | CompletedSignup (Result Http.Error Application)


init : Nav.Key -> Mode -> Model
init key mode =
    { phone = Id ""
    , email = ""
    , code = Id ""
    , plaidToken = Id ""
    , step = EditingForm
    , status = Ready
    , key = key
    , mode = mode
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ plaidLinkExit (always PlaidExited)
        , plaidLinkDone PlaidDone
        ]


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    let
        newApplication token =
            { email = model.email
            , publicBankToken = token
            }
    in
    Debug.log (Debug.toString msg) <|
        case msg of
            EditPhone s ->
                updates { model | phone = Id s }

            EditEmail s ->
                updates { model | email = s }

            SubmitForm ->
                updates { model | status = Loading }
                    |> command (Api.sessionsCreateCode CompletedCreateCode model.phone)

            CompletedCreateCode (Err e) ->
                updates { model | status = Failed "Could not create code" e }

            CompletedCreateCode (Ok _) ->
                updates { model | status = Ready, step = EditingCode }

            EditCode s ->
                updates { model | code = Id s }

            SubmitCode ->
                updates { model | status = Loading }
                    |> command (Api.sessionsCheckCode CompletedCheckCode model.phone model.code)

            CompletedCheckCode (Err e) ->
                updates { model | status = Failed "Invalid code" e }

            CompletedCheckCode (Ok session) ->
                case session.accountId of
                    Nothing ->
                        updates { model | status = Ready, step = Plaid }
                            |> command (plaidLinkOpen Encode.null)

                    Just id ->
                        updates model
                            |> command (Nav.pushUrl model.key (Route.url <| Route.Account id))

            PlaidOpen ->
                updates model
                    |> command (plaidLinkOpen Encode.null)

            PlaidExited ->
                -- Leave this here. Elm throws an error if there are no subscribers for a subscription!
                updates model

            PlaidDone token ->
                updates { model | plaidToken = Id token }
                    |> command (Api.postApplications CompletedSignup <| newApplication (Id token))

            CompletedSignup (Err e) ->
                updates { model | status = Failed "Signup server error" e }

            CompletedSignup (Ok a) ->
                updates model
                    |> command (Nav.pushUrl model.key (Route.url <| Route.Onboard <| Route.Approval <| a.accountId))


view : Model -> Element Msg
view model =
    case model.step of
        EditingForm ->
            case model.mode of
                Signup ->
                    viewSignupForm model

                Login ->
                    viewLoginForm model

        EditingCode ->
            viewPhoneCode model

        Plaid ->
            viewPlaidLanding model


viewSignupForm : Model -> Element Msg
viewSignupForm model =
    column Style.formPage
        [ el Style.header (text "Sign up for Timely")
        , viewEmailInput model
        , viewPhoneInput model
        , Input.button Style.button
            { onPress = Just SubmitForm
            , label = Element.text "Sign up"
            }
        , viewProblems model.status
        ]


viewLoginForm : Model -> Element Msg
viewLoginForm model =
    column Style.formPage
        [ el Style.header (text "Log in to Timely")
        , viewPhoneInput model
        , Input.button Style.button
            { onPress = Just SubmitForm
            , label = Element.text "Login"
            }
        , viewProblems model.status
        ]


viewPhoneInput : Model -> Element Msg
viewPhoneInput model =
    Input.text [ htmlAttribute (Html.type_ "tel") ]
        { text = idValue model.phone
        , placeholder = Nothing
        , onChange = EditPhone
        , label = label "Phone"
        }


viewEmailInput : Model -> Element Msg
viewEmailInput model =
    Input.email []
        { text = model.email
        , placeholder = Nothing
        , onChange = EditEmail
        , label = label "Email"
        }


viewPhoneCode : Model -> Element Msg
viewPhoneCode model =
    column Style.formPage
        [ el Style.header (text "Enter Code")
        , paragraph [] [ text "We sent a message to your phone number" ]
        , Input.text [ htmlAttribute (Html.type_ "tel") ]
            { text = idValue model.code
            , placeholder = Nothing
            , onChange = EditCode
            , label = label "Code"
            }
        , Input.button Style.button
            { onPress = Just SubmitCode
            , label = Element.text "Check code"
            }
        , viewProblems model.status
        ]


viewPlaidLanding : Model -> Element Msg
viewPlaidLanding model =
    column Style.formPage
        [ el Style.header (text "Connect your bank")
        , Input.button Style.button
            { onPress = Just PlaidOpen
            , label = Element.text "Connect Bank"
            }
        , viewProblems model.status
        ]


label : String -> Input.Label Msg
label t =
    Input.labelAbove [ Font.size 14 ] (text t)


viewProblems : Status -> Element Msg
viewProblems status =
    case status of
        Failed p _ ->
            el [ Style.error ] (text p)

        _ ->
            el [] (text "")
