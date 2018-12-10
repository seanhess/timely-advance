module Page.Signup exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input


type alias Model =
    { account : Account }


type alias Account =
    { firstName : String
    , lastName : String
    , email : String
    }


type Msg
    = Update Account


init : Model
init =
    { account = { firstName = "", lastName = "", email = "" } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update a ->
            ( { model | account = a }, Cmd.none )


view : Model -> Element Msg
view model =
    form model.account


form : Account -> Element Msg
form account =
    Element.column [ width (px 800), height shrink, centerY, centerX, spacing 36, padding 10 ]
        -- , explain Debug.todo
        [ text "Hello"
        , Input.text
            [ spacing 12
            , below
                (el
                    [ Font.size 14
                    , alignRight
                    , moveDown 6
                    ]
                    (text "This one is wrong")
                )
            ]
            { text = account.firstName
            , placeholder = Just (Input.placeholder [] (text "First Name "))
            , onChange = \new -> Update { account | firstName = new }
            , label = Input.labelAbove [ Font.size 14 ] (text "First Name")
            }
        , Input.text [ spacing 12, width shrink ]
            { text = account.lastName
            , placeholder = Nothing
            , onChange = \new -> Update { account | lastName = new }
            , label = Input.labelAbove [ Font.size 14 ] (text "Last Name")
            }
        ]


big : List (Attribute msg)
big =
    [ Font.size 18 ]
