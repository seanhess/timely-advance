module Timely.Icons exposing (Size(..), back, cancel, edit, heart, icon, minus, next, plus, profile, size)

import Element exposing (Attribute, Element, column, el, htmlAttribute, row, text)
import Element.Font as Font
import Html.Attributes exposing (class)


type alias Icon msg =
    Attribute msg


type Size
    = Normal
    | Big


icon : Icon msg -> Size -> Element msg
icon ic sz =
    let
        fontSize =
            case sz of
                Normal ->
                    []

                Big ->
                    [ Font.size 32 ]
    in
    el (ic :: fontSize) (text "")


profile : Attribute msg
profile =
    htmlAttribute (class "flaticon-user")


minus : Attribute msg
minus =
    htmlAttribute (class "flaticon-minus")


edit : Attribute msg
edit =
    htmlAttribute (class "flaticon-edit")


heart : Attribute msg
heart =
    htmlAttribute (class "flaticon-heart")


plus : Attribute msg
plus =
    htmlAttribute (class "flaticon-plus")


next : Attribute msg
next =
    htmlAttribute (class "flaticon-next")


cancel : Attribute msg
cancel =
    htmlAttribute (class "flaticon-cancel")


back : Attribute msg
back =
    htmlAttribute (class "flaticon-back")


size =
    Font.size
