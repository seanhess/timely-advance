module Page.Onboard.Phone exposing (Model, Msg, init, subscriptions, update, view)

import Element exposing (Element, column, el, link, padding, row, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Element.Region as Region
import Timely.Style as Style


type alias Model =
    { phone : String
    }


init : Model
init =
    { phone = "" }


type Msg
    = Tapped Int


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Element msg
view _ =
    column [ padding 10, spacing 8 ]
        [ el [ Region.heading 1 ] (text "Phone")
        , column [ spacing 10 ]
            [ row [ spacing 10 ]
                [ button Style.button { onPress = Nothing, label = text "1 " }
                , button Style.button { onPress = Nothing, label = text "1 " }
                ]
            ]
        ]



-- [ button Style.button { onPress = Just (Tapped 1), label = text "1" } ]
