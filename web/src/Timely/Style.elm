module Timely.Style exposing (banner, blue, box, button, dark, darkBlue, darkGreen, destroy, dim, error, formPage, gray, green, header, info, lightBlue, lightRed, link, option, page, primary, red, secondary, section, space, success, white)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html.Attributes as Html


green =
    -- Element.rgb 0.22 0.73 0.39
    -- (Triad complement)
    Element.rgb 0.42 0.62 0.39


darkGreen =
    Element.rgb 0.32 0.52 0.3


lightRed =
    Element.rgb 0.83 0.3 0.34


dark =
    Element.rgb 0.3 0.3 0.3


white =
    Element.rgb 1 1 1


gray =
    Element.rgb 0.9 0.9 0.9


dim =
    Element.rgba 0.3 0.3 0.3 0.5


blue =
    Element.rgb 0 0.4 0.72


lightBlue =
    Element.rgb 0.25 0.6 0.83


red =
    Element.rgb 0.8 0 0


darkBlue =
    Element.rgb 0 0 0.9


button : List (Attribute msg) -> List (Attribute msg)
button styles =
    styles
        ++ [ paddingXY 32 16
           , spacing 10
           , Border.rounded 3
           , width fill
           ]


primary : List (Attribute msg)
primary =
    [ Background.color blue
    , Font.color white
    , Border.color darkBlue
    ]


success : List (Attribute msg)
success =
    [ Background.color green
    , Font.color white
    , Border.color darkGreen
    ]


secondary : List (Attribute msg)
secondary =
    [ Background.color gray
    , Font.color dark
    , Border.color gray
    ]


destroy : List (Attribute msg)
destroy =
    [ Background.color lightRed
    , Font.color white
    , Border.color lightRed
    ]


option : List (Attribute msg)
option =
    [ padding 8
    , spacing 10
    , Border.rounded 3
    ]


formPage : List (Attribute msg)
formPage =
    [ height shrink, centerY, centerX, width (fill |> maximum 800), spacing 36, padding 20 ]


header : List (Attribute msg)
header =
    [ Region.heading 1, Font.size 36, alignLeft ]


banner : List (Attribute msg)
banner =
    [ Region.heading 2, Font.size 28, alignLeft, Background.color gray, width fill, padding 10 ]


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
    [ spacing 20, padding 20, width fill, Background.color white ]


box : Attribute msg
box =
    Border.rounded 3
