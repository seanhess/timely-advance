module Nimble.Style exposing (blue, button, darkBlue, grey, red, white)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region


white =
    Element.rgb 1 1 1


grey =
    Element.rgb 0.9 0.9 0.9


blue =
    Element.rgb 0 0 0.8


red =
    Element.rgb 0.8 0 0


darkBlue =
    Element.rgb 0 0 0.9


button : List (Attribute msg)
button =
    [ Background.color blue
    , Font.color white
    , Border.color darkBlue
    , paddingXY 32 16
    , Border.rounded 3
    , width fill
    ]
