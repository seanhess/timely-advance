module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Page.Onboard as Onboard
import Page.Home as Home
import Url exposing (Url)




-- MODEL -- sub-views? Nested?

type Model
  = NotFound
  | Onboard Onboard.Model
  | Home Home.Model

init : flags -> Url -> Nav.Key -> (Model, Cmd Msg)
init _ _ _ =
  (Onboard Onboard.init, Cmd.none)


-- UPDATE

type Msg
    = Ignored
    -- | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)
  -- case msg of
  --   Increment ->
  --     model + 1

  --   Decrement ->
  --     model - 1


-- VIEW

view : Model -> Document Msg
view model =
  { title = "a title", body = [ div [] [ text "ok" ] ] }
  -- { title = "A title:"
  -- , body = div [] [ text "ok" ]
  -- }

  -- div []
  --   [ button [ onClick Decrement ] [ text "click me" ]
  --   -- , div [] [ text (String.fromInt model) ]
  --   -- , button [ onClick Increment ] [ text "+" ]
  --   ]


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none



main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
