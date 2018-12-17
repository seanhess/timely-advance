module Page.Signup exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Nimble.Api as Api exposing (Account, AccountInfo)
import Nimble.Style as Style
import Route


type alias Model =
    { account : AccountInfo
    , key : Nav.Key
    , status : Status
    }


type Status
    = Editing
    | Loading
    | Complete (List Problem)


type alias Problem =
    String


type Msg
    = Update AccountInfo
    | Submit
    | CompletedSignup (Result Http.Error Account)


init : Nav.Key -> Model
init key =
    { account = { firstName = "", lastName = "", email = "" }, status = Editing, key = key }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update a ->
            ( { model | account = a }, Cmd.none )

        Submit ->
            ( { model | status = Loading }, Api.postAccounts CompletedSignup model.account )

        CompletedSignup (Err e) ->
            ( { model | status = Complete [ "Signup server error" ] }, Cmd.none )

        CompletedSignup (Ok a) ->
            -- TODO redirect to accounts page
            ( { model | status = Complete [] }, Nav.pushUrl model.key (Route.url Route.Accounts) )


view : Model -> Element Msg
view model =
    form model.status model.account


form : Status -> AccountInfo -> Element Msg
form status account =
    Element.column Style.formPage
        [ el Style.header (text "Create an Account")
        , Input.text [ spacing 12 ]
            { text = account.firstName
            , placeholder = Nothing
            , onChange = \new -> Update { account | firstName = new }
            , label = Input.labelAbove [ Font.size 14 ] (text "First Name")
            }
        , Input.text [ spacing 12 ]
            { text = account.lastName
            , placeholder = Nothing
            , onChange = \new -> Update { account | lastName = new }
            , label = Input.labelAbove [ Font.size 14 ] (text "Last Name")
            }
        , Input.email [ spacing 12 ]
            { text = account.email
            , placeholder = Nothing
            , onChange = \new -> Update { account | email = new }
            , label = Input.labelAbove [ Font.size 14 ] (text "Email")
            }
        , submit status
        ]


submit : Status -> Element Msg
submit status =
    case status of
        Editing ->
            Input.button Style.button
                { onPress = Just Submit
                , label = Element.text "Create Account"
                }

        Loading ->
            el [] (text "Loading...")

        Complete problems ->
            el [] (text <| "Complete: " ++ String.concat problems)


big : List (Attribute msg)
big =
    [ Font.size 18 ]
