module Timely.Components exposing (Spinner(..), alert, back, backLink, close, layout, loadingButton, maybe, option, selectBox, spinner, spinnerDots, spinnerRing, spinnerRipple, version)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Html
import Http exposing (Error)
import Route exposing (Route)
import Timely.Icons as Icons
import Timely.Style as Style
import Version


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
        , label = Icons.icon Icons.back Icons.Big
        }


backLink : Route -> Element msg
backLink route =
    Element.link [ Icons.back, Font.size 32 ]
        { url = Route.url route
        , label = text ""
        }


close : msg -> Element msg
close onBack =
    Input.button []
        { onPress = Just onBack
        , label = Icons.icon Icons.cancel Icons.Big
        }



-- Select -------------------------------


selectBox : (Bool -> msg) -> Bool -> Element msg
selectBox onSelect selected =
    let
        color =
            if selected then
                Style.darkBlue

            else
                Style.gray
    in
    Input.button
        [ width (px 20), height (px 20), Background.color color ]
        { onPress = Just (onSelect (not selected))
        , label = none
        }


option : msg -> Bool -> Element msg -> Element msg
option onSelect selected label =
    let
        style =
            if selected then
                Style.primary

            else
                Style.secondary
    in
    Input.button (Style.button style ++ Style.option)
        { onPress = Just onSelect, label = label }



-- Disabled Button -------------------------------


submitButton : msg -> Bool -> Element msg -> Element msg
submitButton onMsg enabled label =
    let
        style =
            if enabled then
                Style.primary

            else
                Style.secondary

        action =
            if enabled then
                Just onMsg

            else
                Nothing
    in
    Input.button (Style.button style)
        { onPress = action
        , label = label
        }


version : String -> Element msg
version loaded =
    el
        [ padding 4
        , Font.size 12
        , Font.color (Element.rgb 0.7 0.7 0.7)

        -- , alignRight
        ]
        (text <| "Loaded: " ++ loaded ++ " - Version: " ++ Version.version)


maybe : (a -> Element msg) -> Maybe a -> Element msg
maybe view ma =
    case ma of
        Nothing ->
            Element.none

        Just a ->
            view a


alert : List (Element msg) -> Element msg
alert =
    column
        [ spacing 10
        , padding 14
        , width fill
        , Background.color Style.red
        , Font.color Style.white
        , Style.box
        ]


layout : String -> List (Attribute msg) -> Element msg -> Html msg
layout loaded atts pageView =
    Element.layout atts
        (Element.column
            [ width fill
            , height fill
            ]
            [ pageView
            , version loaded
            ]
        )
