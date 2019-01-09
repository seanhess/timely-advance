port module Page.Signup exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Encode as Encode
import Nimble.Api as Api exposing (AccountInfo, Application)
import Nimble.Style as Style
import Platform.Updates exposing (Updates, base, command, set)
import Route


port plaidLinkOpen : Encode.Value -> Cmd msg


port plaidLinkExit : (Encode.Value -> msg) -> Sub msg


port plaidLinkDone : (String -> msg) -> Sub msg


type alias Form =
    { firstName : String
    , lastName : String
    , email : String
    }


type alias Model =
    { form : Form
    , key : Nav.Key
    , status : Status
    }


type alias PublicToken =
    String


type Status
    = Editing
    | Plaid
    | Saving PublicToken
    | Complete (List Problem)


type alias Problem =
    String


type Msg
    = Update Form
    | Submit
    | PlaidExited
    | PlaidDone String
    | CompletedSignup (Result Http.Error Application)


init : Nav.Key -> Model
init key =
    { form = { firstName = "", lastName = "", email = "" }
    , status = Editing
    , key = key
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ plaidLinkExit (always PlaidExited)
        , plaidLinkDone PlaidDone
        ]


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    let
        updates =
            base model

        newApplication token =
            { firstName = model.form.firstName
            , lastName = model.form.lastName
            , email = model.form.email
            , publicBankToken = token
            }
    in
    case msg of
        Update f ->
            updates
                |> set { model | form = f }

        Submit ->
            updates
                |> set { model | status = Plaid }
                |> command (plaidLinkOpen Encode.null)

        PlaidExited ->
            updates
                |> set { model | status = Editing }

        PlaidDone token ->
            updates
                |> set { model | status = Saving token }
                |> command (Api.postApplications CompletedSignup <| newApplication token)

        CompletedSignup (Err e) ->
            updates
                |> set { model | status = Complete [ "Signup server error" ] }

        CompletedSignup (Ok a) ->
            updates
                |> set { model | status = Complete [] }
                |> command (Nav.pushUrl model.key (Route.url <| Route.Account <| a.accountId))


view : Model -> Element Msg
view model =
    form model.status model.form


form : Status -> Form -> Element Msg
form status frm =
    Element.column Style.formPage
        [ el Style.header (text "Create an Account")
        , Input.text [ spacing 12 ]
            { text = frm.firstName
            , placeholder = Nothing
            , onChange = \new -> Update { frm | firstName = new }
            , label = Input.labelAbove [ Font.size 14 ] (text "First Name")
            }
        , Input.text [ spacing 12 ]
            { text = frm.lastName
            , placeholder = Nothing
            , onChange = \new -> Update { frm | lastName = new }
            , label = Input.labelAbove [ Font.size 14 ] (text "Last Name")
            }
        , Input.email [ spacing 12 ]
            { text = frm.email
            , placeholder = Nothing
            , onChange = \new -> Update { frm | email = new }
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

        Plaid ->
            el [] (text "Plaid...")

        Saving token ->
            el [] (text <| "Saving: " ++ token)

        Complete problems ->
            el [] (text <| "Complete: " ++ String.concat problems)


big : List (Attribute msg)
big =
    [ Font.size 18 ]
