port module Page.Onboard.Signup exposing (Model, Msg, init, subscriptions, update, view)

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
    , authToken : Token Auth
    , plaidToken : Token Bank
    , key : Nav.Key
    , status : Status
    , isLoading : Bool
    , problems : List Problem
    }


type Status
    = EditingForm
    | EditingCode
    | Plaid


type alias Problem =
    String


type Msg
    = EditPhone String
    | EditEmail String
    | SubmitForm
    | EditCode String
    | SubmitCode
    | PlaidOpen
    | PlaidExited
    | PlaidDone String
    | CompletedCheckCode (Result Http.Error (Token Auth))
    | CompletedCreateCode (Result Http.Error (Id Session))
    | CompletedSignup (Result Http.Error Application)


init : Nav.Key -> Model
init key =
    { phone = Id ""
    , email = ""
    , code = Id ""
    , authToken = Id ""
    , plaidToken = Id ""
    , status = EditingForm
    , key = key
    , problems = []
    , isLoading = False
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
    case msg of
        EditPhone s ->
            updates { model | phone = Id s }

        EditEmail s ->
            updates { model | email = s }

        SubmitForm ->
            updates { model | status = EditingCode }
                |> command (Api.sessionsCreateCode CompletedCreateCode model.phone)

        CompletedCreateCode (Err e) ->
            updates { model | problems = [ "Could not create code" ] }

        CompletedCreateCode (Ok _) ->
            updates model

        EditCode s ->
            updates { model | code = Id s }

        SubmitCode ->
            updates { model | isLoading = True }
                |> command (Api.sessionsCheckCode CompletedCheckCode model.phone model.code)

        CompletedCheckCode (Err e) ->
            updates { model | problems = [ "Invalid code" ] }

        CompletedCheckCode (Ok t) ->
            updates { model | authToken = t, status = Plaid }
                |> command (plaidLinkOpen Encode.null)

        PlaidOpen ->
            updates model
                |> command (plaidLinkOpen Encode.null)

        -- Leave this here. Elm throws an error if there are no subscribers for a subscription!
        PlaidExited ->
            updates model

        PlaidDone token ->
            updates { model | plaidToken = Id token }
                |> command (Api.postApplications CompletedSignup <| newApplication (Id token))

        CompletedSignup (Err e) ->
            updates { model | problems = [ "Signup server error: " ++ Debug.toString e ] }

        CompletedSignup (Ok a) ->
            updates model
                |> command (Nav.pushUrl model.key (Route.url <| Route.Onboard <| Route.Approval <| a.accountId))


view : Model -> Element Msg
view model =
    case model.status of
        EditingForm ->
            viewSignupForm model

        EditingCode ->
            viewPhoneCode model

        Plaid ->
            viewPlaidLanding


viewSignupForm : Model -> Element Msg
viewSignupForm model =
    column Style.formPage
        [ el Style.header (text "Sign up for Timely")
        , Input.email []
            { text = model.email
            , placeholder = Nothing
            , onChange = EditEmail
            , label = label "Email"
            }
        , Input.text [ htmlAttribute (Html.type_ "tel") ]
            { text = idValue model.phone
            , placeholder = Nothing
            , onChange = EditPhone
            , label = label "Phone"
            }
        , Input.button Style.button
            { onPress = Just SubmitForm
            , label = Element.text "Sign up"
            }
        ]


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
        ]


viewPlaidLanding : Element Msg
viewPlaidLanding =
    column Style.formPage
        [ el Style.header (text "Connect your bank")
        , Input.button Style.button
            { onPress = Just PlaidOpen
            , label = Element.text "Connect Bank"
            }
        ]


label : String -> Input.Label Msg
label t =
    Input.labelAbove [ Font.size 14 ] (text t)



-- TODO collect email and phone
-- TODO collect phone code
-- TODO show plaid
-- TODO bank landing page "Securly Connect bank"
