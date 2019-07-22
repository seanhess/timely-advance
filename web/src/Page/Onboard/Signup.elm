module Page.Onboard.Signup exposing (Mode(..), Model, Msg, init, subscriptions, update, view)

-- import Debug

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
import Timely.Types exposing (Auth, AuthCode, Email, Id(..), Phone, Token, Valid(..), idValue)
import Timely.Types.Account exposing (AccountId)
import Timely.Types.Application exposing (AccountInfo, Application, Bank)
import Timely.Types.Date as Date exposing (Date)


type alias Model =
    { now : Date
    , email : Email
    , phone : Phone
    , code : Token AuthCode
    , mode : Mode
    , step : Step
    }



-- what are the states?


type Step
    = StepPhone (Resource ())
    | StepCode (Resource (Maybe (Id AccountId)))


type Mode
    = Signup
    | Login


type Msg
    = Back
    | BackPhone
    | OnDate Date
    | EditPhone String
    | EditEmail String
    | SubmitPhone
    | EditCode String
    | SubmitCode
    | OnCheckCode (Result Http.Error Session)
    | OnCreateCode (Result Http.Error ())


init : Mode -> ( Model, Cmd Msg )
init mode =
    ( { now = Date.empty
      , phone = Id ""
      , email = ""
      , code = Id ""
      , mode = mode
      , step = StepPhone Init
      }
    , Date.current OnDate
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update key msg model =
    let
        newApplication : Id Bank -> AccountInfo
        newApplication token =
            { email = model.email
            , publicBankToken = token
            }
    in
    case msg of
        OnDate t ->
            updates { model | now = t }

        Back ->
            updates model
                |> command (Route.pushUrl key <| Route.Onboard Route.Landing)

        BackPhone ->
            updates { model | step = StepPhone Init }

        EditPhone s ->
            updates { model | phone = Id s }

        EditEmail s ->
            updates { model | email = s }

        SubmitPhone ->
            updates { model | step = StepPhone Loading }
                |> command (Api.sessionsCreateCode OnCreateCode model.phone)

        OnCreateCode (Err e) ->
            updates { model | step = StepPhone <| Failed e }

        OnCreateCode (Ok _) ->
            updates { model | step = StepCode Init }

        EditCode s ->
            updates { model | code = Id s }

        SubmitCode ->
            updates { model | step = StepCode Loading }
                |> command (Api.sessionsCheckCode OnCheckCode model.phone model.code)

        OnCheckCode (Err e) ->
            updates { model | step = StepCode <| Failed e }

        OnCheckCode (Ok session) ->
            updates { model | step = StepCode <| Ready session.accountId }
                |> command
                    (case session.accountId of
                        Just id ->
                            -- If they have an accountId, redirect them to their account, even if they were trying to sign up
                            Route.pushUrl key (Route.Account id <| Route.AccountMain)

                        Nothing ->
                            case model.mode of
                                -- If they were signing up, continue to bank
                                Signup ->
                                    Route.pushUrl key <| Route.Onboard <| Route.Bank model.email

                                Login ->
                                    Cmd.none
                    )


view : Model -> Element Msg
view model =
    case model.step of
        StepPhone genCode ->
            case model.mode of
                Signup ->
                    viewSignupForm model genCode

                Login ->
                    viewLoginForm model genCode

        StepCode accountId ->
            viewPhoneCode model accountId


viewSignupForm : Model -> Resource () -> Element Msg
viewSignupForm model genCode =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back Back
                , paragraph Style.heading [ text "Let's get started!" ]
                ]
            ]
        , column Style.section
            [ paragraph [] [ text "What's your number?" ]
            , paragraph [ Font.size 16 ] [ text "We will use your number to text you when your balance is at risk" ]
            , viewPhoneInput model
            , paragraph [] [ text "What's your email?" ]
            , viewEmailInput model
            , Components.loadingButton (Style.button Style.primary)
                { onPress = SubmitPhone
                , label =
                    column [ spacing 8, width fill ]
                        [ el [ Font.bold, centerX ] (text "Join Timely Advance")
                        , el [ centerX ] (text "Costs $1/month")
                        ]
                , isLoading = Resource.isLoading genCode
                }
            , viewProblems "Could not generate code" genCode
            , paragraph [ Font.size 12 ] [ text "By joining, I agree to Timely Advance's Privacy Policy, TOS, Payment Authorization & Electronic Communication Consent" ]
            ]
        ]


viewLoginForm : Model -> Resource () -> Element Msg
viewLoginForm model genCode =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back Back
                , paragraph Style.heading [ text "Welcome back!" ]
                ]
            ]
        , column Style.section
            [ paragraph [] [ text "Please enter your phone number" ]
            , viewPhoneInput model
            , Components.loadingButton (Style.button Style.primary)
                { onPress = SubmitPhone
                , label = Style.label "Login"
                , isLoading = Resource.isLoading genCode
                }
            , viewProblems "Could not generate code" genCode
            ]
        ]


viewPhoneInput : Model -> Element Msg
viewPhoneInput model =
    Input.text [ htmlAttribute (Html.type_ "tel") ]
        { text = idValue model.phone
        , placeholder = Just (Input.placeholder [] (text "Enter Mobile Number"))
        , onChange = EditPhone
        , label = Input.labelHidden "Mobile Number"
        }


viewEmailInput : Model -> Element Msg
viewEmailInput model =
    Input.email []
        { text = model.email
        , placeholder = Just (Input.placeholder [] (text "Personal Email Address"))
        , onChange = EditEmail
        , label = Input.labelHidden "Email"
        }



-- when they click back here, we need to go back a step


viewPhoneCode : Model -> Resource (Maybe (Id AccountId)) -> Element Msg
viewPhoneCode model accountId =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back BackPhone
                , paragraph [] [ text "We just sent you a text" ]
                ]
            , paragraph [] [ text <| "Enter the 4 digit code we sent to: " ++ idValue model.phone ]
            ]
        , column Style.section
            [ Input.text [ htmlAttribute (Html.type_ "tel") ]
                { text = idValue model.code
                , placeholder = Just <| Input.placeholder [] (text "Enter 4 digit code")
                , onChange = EditCode
                , label = Input.labelHidden "Code"
                }
            , Components.loadingButton (Style.button Style.primary)
                { onPress = SubmitCode
                , label = Style.label "Next"
                , isLoading = Resource.isLoading accountId
                }
            , viewProblems "Could not validate code" accountId
            , case accountId of
                Ready Nothing ->
                    el [ Style.error ] (text "User not found")

                _ ->
                    Element.none
            ]
        ]


viewProblems : String -> Resource a -> Element Msg
viewProblems message resource =
    case resource of
        Failed _ ->
            el [ Style.error ] (text message)

        _ ->
            Element.none



-- Identity ---------------------
-- viewIdentityForm : Model -> Element Msg
-- viewIdentityForm model =
--     column Style.page
--         [ column Style.info
--             [ row [ spacing 15 ]
--                 [ Components.back (Step Plaid)
--                 , paragraph [] [ text "Please give us a few more details" ]
--                 ]
--             ]
--         , column Style.section
--             [ viewEmailInput model
--             , viewSSNInput model
--             , viewDobInput model
--             , Components.loadingButton (Style.button Style.primary)
--                 { onPress = SubmitIdentity
--                 , label = Element.text "Finish"
--                 , isLoading = model.status == Loading
--                 }
--             , viewProblems model.status
--             ]
--         ]
-- viewSSNInput : Model -> Element Msg
-- viewSSNInput model =
--     Input.text []
--         { text = model.ssn
--         , placeholder = Nothing
--         , onChange = EditSSN
--         , label = label "SSN (9 digits)"
--         }
-- viewDobInput : Model -> Element Msg
-- viewDobInput model =
--     Input.text
--         [ htmlAttribute (Html.type_ "date")
--         , htmlAttribute (Html.min "1900-01-01")
--         , htmlAttribute (Html.max (Date.toIsoString model.now))
--         , htmlAttribute (Html.style "flex-direction" "row")
--         ]
--         { text = model.dob
--         , placeholder = Nothing
--         , onChange = EditDob
--         , label = label "Date of Birth"
--         }
