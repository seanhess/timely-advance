module Page.Signup exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Nimble.Style as Style


type alias Model =
    { account : Account }


type alias Account =
    { firstName : String
    , lastName : String
    , email : String
    }


type Msg
    = Update Account
    | Submit


init : Model
init =
    { account = { firstName = "", lastName = "", email = "" } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update a ->
            ( { model | account = a }, Cmd.none )

        Submit ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    form model.account


form : Account -> Element Msg
form account =
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
        , Input.button Style.button
            { onPress = Just Submit
            , label = Element.text "Create Account"
            }
        ]


big : List (Attribute msg)
big =
    [ Font.size 18 ]
