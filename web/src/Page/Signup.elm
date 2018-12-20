module Page.Signup exposing (Evt(..), Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Nimble.Api as Api exposing (Account, AccountInfo)
import Nimble.Style as Style
import Route


type alias Model =
    { account : AccountInfo
    , key : Nav.Key
    , status : Status
    }


type Status
    = Editing
    | Loading
    | Complete (List Problem)


type alias Problem =
    String


type Msg
    = Update AccountInfo
    | Submit
    | CompletedSignup (Result Http.Error Account)


type Evt
    = PlaidLinkOpen


init : Nav.Key -> Model
init key =
    { account = { firstName = "", lastName = "", email = "" }
    , status = Editing
    , key = key
    }



-- now it can run global commands, no?
-- we can have different types of events?
-- can we add events with .?


type alias Updates model msg event =
    ( model, Cmd msg, Event event )


type alias Event a =
    Maybe a


base : model -> Updates model msg event
base mod =
    ( mod, Cmd.none, Nothing )


set : model -> Updates model msg event -> Updates model msg event
set mod ( _, msg, ev ) =
    ( mod, msg, ev )


modify : (model -> model) -> Updates model msg event -> Updates model msg event
modify up ( mod, cmd, ev ) =
    ( up mod, cmd, ev )


command : Cmd msg -> Updates model msg event -> Updates model msg event
command cmd2 ( mod, cmd1, ev ) =
    ( mod, Cmd.batch [ cmd1, cmd2 ], ev )


event : evt -> Updates model msg evt -> Updates model msg evt
event evt ( mod, cmd, _ ) =
    ( mod, cmd, Just evt )


update : Msg -> Model -> Updates Model Msg Evt
update msg model =
    let
        updates =
            base model
    in
    case msg of
        Update a ->
            updates
                |> set { model | account = a }

        Submit ->
            updates
                |> set { model | status = Loading }
                |> event PlaidLinkOpen

        CompletedSignup (Err e) ->
            updates
                |> set { model | status = Complete [ "Signup server error" ] }

        CompletedSignup (Ok a) ->
            -- TODO redirect to accounts page
            updates
                |> set { model | status = Complete [] }
                |> command (Nav.pushUrl model.key (Route.url Route.Accounts))


view : Model -> Element Msg
view model =
    form model.status model.account


form : Status -> AccountInfo -> Element Msg
form status account =
    Element.column Style.formPage
        [ el Style.header (text "Create an Account")
        , Input.text [ spacing 12 ]
            { text = account.firstName
            , placeholder = Nothing
            , onChange = \new -> Update { account | firstName = new }
            , label = Input.labelAbove [ Font.size 14 ] (text "First Name")
            }
        , Input.text [ spacing 12 ]
            { text = account.lastName
            , placeholder = Nothing
            , onChange = \new -> Update { account | lastName = new }
            , label = Input.labelAbove [ Font.size 14 ] (text "Last Name")
            }
        , Input.email [ spacing 12 ]
            { text = account.email
            , placeholder = Nothing
            , onChange = \new -> Update { account | email = new }
            , label = Input.labelAbove [ Font.size 14 ] (text "Email")
            }
        , submit status
        ]


submit : Status -> Element Msg
submit status =
    case status of
        Editing ->
            Input.button Style.button
                { onPress = Just Submit
                , label = Element.text "Create Account"
                }

        Loading ->
            el [] (text "Loading...")

        Complete problems ->
            el [] (text <| "Complete: " ++ String.concat problems)


big : List (Attribute msg)
big =
    [ Font.size 18 ]
