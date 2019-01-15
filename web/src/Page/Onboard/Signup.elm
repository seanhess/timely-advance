port module Page.Onboard.Signup exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Html
import Http
import Json.Encode as Encode
import Platform.Updates exposing (Updates, command, set, updates)
import Route
import Timely.Api as Api exposing (AccountInfo, Application)
import Timely.Components exposing (loadingButton)
import Timely.Style as Style


port plaidLinkOpen : Encode.Value -> Cmd msg


port plaidLinkExit : (Encode.Value -> msg) -> Sub msg


port plaidLinkDone : (String -> msg) -> Sub msg


type alias Form =
    { email : String
    , phone : String
    }


type alias Model =
    { form : Form
    , code : String
    , key : Nav.Key
    , status : Status
    }


type alias PublicToken =
    String


type Status
    = Editing
    | Plaid
    | Saving PublicToken
    | Complete (List Problem)


type alias Problem =
    String


type Msg
    = Update Form
    | EditCode String
    | Submit
    | PlaidExited
    | PlaidDone String
    | CompletedSignup (Result Http.Error Application)


init : Nav.Key -> Model
init key =
    { form = { phone = "", email = "" }
    , code = ""
    , status = Editing
    , key = key
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
            { email = model.form.email
            , phone = model.form.phone
            , publicBankToken = token
            }
    in
    case msg of
        Update f ->
            updates { model | form = f }

        Submit ->
            updates { model | status = Plaid }
                |> command (plaidLinkOpen Encode.null)

        PlaidExited ->
            updates { model | status = Editing }

        PlaidDone token ->
            updates { model | status = Saving token }
                |> command (Api.postApplications CompletedSignup <| newApplication token)

        CompletedSignup (Err e) ->
            updates { model | status = Complete [ "Signup server error" ] }

        CompletedSignup (Ok a) ->
            updates { model | status = Complete [] }
                |> command (Nav.pushUrl model.key (Route.url <| Route.Onboard <| Route.Approval <| a.accountId))

        EditCode s ->
            updates { model | code = s }


view : Model -> Element Msg
view model =
    viewSignupForm model.status model.form


viewSignupForm : Status -> Form -> Element Msg
viewSignupForm status frm =
    Element.column Style.formPage
        [ el Style.header (text "Create an Account")
        , Input.email []
            { text = frm.email
            , placeholder = Nothing
            , onChange = \new -> Update { frm | email = new }
            , label = label "Email"
            }
        , Input.text [ htmlAttribute (Html.type_ "tel") ]
            { text = frm.phone
            , placeholder = Nothing
            , onChange = \new -> Update { frm | phone = new }
            , label = label "Phone"
            }
        , loadingButton Style.button
            { onPress = Submit
            , label = Element.text "Create Account"
            , isLoading = status /= Editing
            }
        ]


viewPhoneCode : String -> Element Msg
viewPhoneCode code =
    column []
        [ el Style.header (text "Enter Code")
        , paragraph [] [ text "We sent a message to your phone number" ]
        , Input.text [ htmlAttribute (Html.type_ "tel") ]
            { text = code
            , placeholder = Nothing
            , onChange = EditCode
            , label = label "Phone"
            }
        ]


label : String -> Input.Label Msg
label t =
    Input.labelAbove [ Font.size 14 ] (text t)
