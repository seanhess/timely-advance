module Page.Accounts exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Nimble.Api exposing (Account)
import Nimble.Style as Style


type alias Model =
    { accounts : List Account }


type Msg
    = None


init : Model
init =
    { accounts = [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- case msg of
--     Update a ->
--         ( { model | account = a }, Cmd.none )
--     Submit ->
--         ( model, Cmd.none )


view : Model -> Element Msg
view model =
    Element.column Style.formPage
        [ el Style.header (text "Hello") ]
