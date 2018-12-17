module Page.Accounts exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Nimble.Api as Api exposing (Account)
import Nimble.Style as Style


type alias Model =
    { accounts : List Account
    , status : Status
    }


type alias Problem =
    String


type Status
    = Loading
    | Complete (List Problem)


type Msg
    = LoadComplete (Result Http.Error (List Account))


init : ( Model, Cmd Msg )
init =
    ( { accounts = [], status = Loading }, Api.getAccounts LoadComplete )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadComplete (Err e) ->
            ( { model | status = Complete [ "Loading error" ] }, Cmd.none )

        LoadComplete (Ok accounts) ->
            ( { model | accounts = accounts, status = Complete [] }, Cmd.none )


view : Model -> Element Msg
view model =
    Element.column Style.formPage
        [ el Style.header (text "Accounts")
        , Element.column [] (List.map accountView model.accounts)
        ]


accountView : Account -> Element Msg
accountView account =
    Element.row [ spacing 8, padding 4 ]
        [ el [] (text account.accountId)
        , el [] (text account.firstName)
        , el [] (text account.lastName)
        , el [] (text account.email)
        ]
