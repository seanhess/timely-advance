module Timely.Style exposing (blue, button, dark, darkBlue, error, formPage, grey, header, info, lightBlue, link, page, red, section, white)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html.Attributes as Html


dark =
    Element.rgb 0.3 0.3 0.3


white =
    Element.rgb 1 1 1


grey =
    Element.rgb 0.9 0.9 0.9


blue =
    Element.rgb 0 0 0.8


lightBlue =
    Element.rgb 0.25 0.6 0.83


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


formPage : List (Attribute msg)
formPage =
    [ height shrink, centerY, centerX, width (fill |> maximum 800), spacing 36, padding 20 ]


header : List (Attribute msg)
header =
    [ Region.heading 1, Font.size 36, alignLeft ]


error : Attribute msg
error =
    Font.color red


link : Attribute msg
link =
    Font.underline


page : List (Attribute msg)
page =
    [ height fill, width fill ]


space : List (Attribute msg)
space =
    [ height fill, width fill ]


info : List (Attribute msg)
info =
    [ spacing 20
    , padding 20
    , height fill
    , width fill
    , htmlAttribute (Html.style "border-top-left-radius" "10px")
    , htmlAttribute (Html.style "border-top-right-radius" "10px")
    , Background.color lightBlue
    , Font.color white
    ]


section : List (Attribute msg)
section =
    [ spacing 20, padding 20, width fill ]
