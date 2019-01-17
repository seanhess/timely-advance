module Page.Onboard.Landing exposing (Model, Msg, init, update, view)

import Element exposing (Element, column, el, link, padding, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Route exposing (Onboard(..), Route(..))
import Timely.Api as Api
import Timely.Style as Style


type alias Model =
    { login : String
    , logout : String
    , test : String
    }


init : ( Model, Cmd Msg )
init =
    ( { login = "", test = "", logout = "" }, Cmd.none )


type Msg
    = Login
    | LoginDone (Result Http.Error ())
    | Test
    | TestDone (Result Http.Error String)
    | Logout
    | LogoutDone (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            ( model, Api.postLogin LoginDone )

        LoginDone res ->
            Debug.log (Debug.toString res) <|
                ( { model | login = Debug.toString res }, Cmd.none )

        Logout ->
            ( model, Api.postLogout LogoutDone )

        LogoutDone res ->
            Debug.log (Debug.toString res) <|
                ( { model | logout = Debug.toString res }, Cmd.none )

        Test ->
            ( model, Api.getTest TestDone )

        TestDone res ->
            Debug.log (Debug.toString res) <|
                ( { model | test = Debug.toString res }, Cmd.none )


view : Model -> Element Msg
view model =
    column [ padding 10, spacing 8 ]
        [ el [ Region.heading 1 ] (text "Timely Advance")
        , link Style.button { url = Route.url (Onboard Signup), label = text "Sign Up" }
        , Input.button Style.button { onPress = Just Login, label = text "Login" }
        , el [] (text <| "Login: " ++ model.login)
        , Input.button Style.button { onPress = Just Test, label = text "Test" }
        , el [] (text <| "Test :" ++ model.test)
        , Input.button Style.button { onPress = Just Logout, label = text "Logout" }
        , el [] (text <| "Logout :" ++ model.logout)
        ]
