port module Page.Onboard.Signup exposing (Mode(..), Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Date exposing (Date)
import Debug
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
import Timely.Api as Api exposing (AccountInfo, Application, Auth, AuthCode, Bank, Id(..), Phone, SSN, Session, Token, Valid(..), idValue)
import Timely.Components as Components exposing (loadingButton)
import Timely.Style as Style


port plaidLinkOpen : Encode.Value -> Cmd msg


port plaidLinkExit : (Encode.Value -> msg) -> Sub msg


port plaidLinkDone : (String -> msg) -> Sub msg


type alias Model =
    { now : String
    , email : String
    , phone : Phone
    , dob : String
    , ssn : String
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
    = EditingPhone
    | EditingCode
    | EditingIdentity
    | Plaid


type Status
    = Loading
    | Ready
    | Failed String Http.Error


type Msg
    = EditPhone String
    | EditEmail String
    | EditDob String
    | EditSSN String
    | Navigate Route
    | OnTime Time.Posix
    | SubmitForm
    | EditCode String
    | SubmitCode
    | SubmitIdentity
    | Step Step
    | PlaidOpen
    | PlaidExited
    | PlaidDone String
    | CompletedCheckCode (Result Http.Error Session)
    | CompletedCreateCode (Result Http.Error ())
    | CompletedSignup (Result Http.Error Application)


init : Nav.Key -> Mode -> ( Model, Cmd Msg )
init key mode =
    ( { now = ""
      , phone = Id ""
      , email = ""
      , ssn = ""
      , dob = "2000-01-01"
      , code = Id ""
      , plaidToken = Id ""
      , step = EditingPhone
      , status = Ready
      , key = key
      , mode = mode
      }
    , Time.now |> Task.perform OnTime
    )


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
            , ssn = Valid model.ssn
            , dateOfBirth = model.dob
            , publicBankToken = token
            }
    in
    -- Debug.log (Debug.toString msg) <|
    case msg of
        OnTime t ->
            updates { model | now = Date.toDateString t }

        Navigate route ->
            updates model
                |> command (Route.pushUrl model.key route)

        Step step ->
            updates { model | step = step }

        EditPhone s ->
            updates { model | phone = Id s }

        EditEmail s ->
            updates { model | email = s }

        EditSSN s ->
            updates { model | ssn = s }

        EditDob d ->
            updates { model | dob = d }

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
                        |> command (Route.pushUrl model.key (Route.Account id <| Route.AccountMain))

        PlaidOpen ->
            updates model
                |> command (plaidLinkOpen Encode.null)

        PlaidExited ->
            -- Leave this here. Elm throws an error if there are no subscribers for a subscription!
            updates model

        PlaidDone token ->
            -- TODO validation
            updates { model | plaidToken = Id token, step = EditingIdentity }

        SubmitIdentity ->
            updates model
                |> command (Api.postApplications CompletedSignup <| newApplication model.plaidToken)

        CompletedSignup (Err e) ->
            updates { model | status = Failed "Signup server error" e }

        CompletedSignup (Ok a) ->
            updates model
                |> command (Nav.pushUrl model.key (Route.url <| Route.Onboard <| Route.Approval <| a.accountId))


view : Model -> Element Msg
view model =
    case model.step of
        EditingPhone ->
            case model.mode of
                Signup ->
                    viewSignupForm model

                Login ->
                    viewLoginForm model

        EditingCode ->
            viewPhoneCode model

        EditingIdentity ->
            viewIdentityForm model

        Plaid ->
            viewPlaidLanding model



-- formPage : List (Attribute msg)
-- formPage =
--     [ height shrink, centerY, centerX, width (fill |> maximum 800), spacing 36, padding 20 ]


viewSignupForm : Model -> Element Msg
viewSignupForm model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back <| Navigate (Route.Onboard Route.Landing)
                , paragraph [] [ text "Start by entering your phone number" ]
                ]
            ]
        , column Style.section
            [ viewPhoneInput model
            , Input.button (Style.button Style.primary)
                { onPress = Just SubmitForm
                , label = Element.text "Join Timely"
                }
            , viewProblems model.status
            ]
        ]


viewLoginForm : Model -> Element Msg
viewLoginForm model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back <| Navigate (Route.Onboard Route.Landing)
                , paragraph [] [ text "Welcome back! Please enter your phone number" ]
                ]
            ]
        , column Style.section
            [ viewPhoneInput model
            , Input.button (Style.button Style.primary)
                { onPress = Just SubmitForm
                , label = Element.text "Login"
                }
            , viewProblems model.status
            ]
        ]


viewPhoneInput : Model -> Element Msg
viewPhoneInput model =
    Input.text [ htmlAttribute (Html.type_ "tel") ]
        { text = idValue model.phone
        , placeholder = Just <| Input.placeholder [] (text "Enter your mobile number")
        , onChange = EditPhone
        , label = Input.labelHidden "Phone"
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
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back (Step EditingPhone)
                , paragraph [] [ text "We sent a message to your phone number, please enter the code below" ]
                ]
            ]
        , column Style.section
            [ Input.text [ htmlAttribute (Html.type_ "tel") ]
                { text = idValue model.code
                , placeholder = Just <| Input.placeholder [] (text "Enter 4 digit code")
                , onChange = EditCode
                , label = Input.labelHidden "Code"
                }
            , Input.button (Style.button Style.primary)
                { onPress = Just SubmitCode
                , label = Element.text "Check code"
                }
            , viewProblems model.status
            ]
        ]


viewPlaidLanding : Model -> Element Msg
viewPlaidLanding model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back (Step EditingCode)
                , paragraph [] [ text "Connect your bank" ]
                ]
            ]
        , column Style.section
            [ Input.button (Style.button Style.primary)
                { onPress = Just PlaidOpen
                , label = Element.text "Connect Bank"
                }
            , viewProblems model.status
            ]
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



-- Identity ---------------------


viewIdentityForm : Model -> Element Msg
viewIdentityForm model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back (Step Plaid)
                , paragraph [] [ text "Please give us a few more details" ]
                ]
            ]
        , column Style.section
            [ viewEmailInput model
            , viewSSNInput model
            , viewDobInput model
            , Input.button (Style.button Style.primary)
                { onPress = Just SubmitIdentity
                , label = Element.text "Finish"
                }
            , viewProblems model.status
            ]
        ]


viewSSNInput : Model -> Element Msg
viewSSNInput model =
    Input.text []
        { text = model.ssn
        , placeholder = Nothing
        , onChange = EditSSN
        , label = label "SSN (9 digits)"
        }


viewDobInput : Model -> Element Msg
viewDobInput model =
    Input.text
        [ htmlAttribute (Html.type_ "date")
        , htmlAttribute (Html.min "1900-01-01")
        , htmlAttribute (Html.max model.now)
        , htmlAttribute (Html.style "flex-direction" "row")
        ]
        { text = model.dob
        , placeholder = Nothing
        , onChange = EditDob
        , label = label "Date of Birth"
        }
