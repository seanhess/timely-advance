module Timely.Components exposing (loadingButton, spinnerRing, spinnerRipple)

import Element exposing (..)
import Element.Input as Element
import Html
import Html.Attributes as Html


type Spinner
    = Ripple
    | Ring
    | Dots


spinnerRipple : Element msg
spinnerRipple =
    Element.html <|
        Html.div
            [ Html.class "lds-ripple" ]
            [ Html.div [] [], Html.div [] [] ]


spinnerRing : Element msg
spinnerRing =
    Element.html <|
        Html.div
            [ Html.class "lds-ring" ]
            [ Html.div [] [], Html.div [] [], Html.div [] [], Html.div [] [] ]


spinnerDots : Element msg
spinnerDots =
    Element.html <|
        Html.div
            [ Html.class "lds-ellipsis" ]
            [ Html.div [] [], Html.div [] [], Html.div [] [], Html.div [] [] ]



-- until I get a design: easier to just put a loader above everything


loadingButton : List (Attribute msg) -> { onPress : msg, label : Element msg, isLoading : Bool } -> Element msg
loadingButton atts info =
    if info.isLoading then
        Element.button (atts ++ [ Element.alpha 0.5 ])
            { onPress = Nothing
            , label = Element.el [ height (shrink |> maximum 20), moveUp 20, centerX ] spinnerRipple
            }

    else
        Element.button atts
            { onPress = Just info.onPress
            , label = info.label
            }
