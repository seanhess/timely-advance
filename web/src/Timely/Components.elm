module Timely.Components exposing (Spinner(..), back, loadingButton, spinner, spinnerDots, spinnerRing, spinnerRipple)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as Html
import Http exposing (Error)
import Route exposing (Route)


type Spinner
    = Ripple
    | Ring
    | Dots


spinner =
    spinnerRipple


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
        Input.button (atts ++ [ Element.alpha 0.5 ])
            { onPress = Nothing
            , label = Element.el [ height (shrink |> maximum 20), moveUp 20, centerX ] spinnerRipple
            }

    else
        Input.button atts
            { onPress = Just info.onPress
            , label = info.label
            }


back : msg -> Element msg
back onBack =
    Input.button []
        { onPress = Just onBack
        , label =
            Element.el
                [ rotate pi, Font.size 32 ]
                (text "➜")
        }
