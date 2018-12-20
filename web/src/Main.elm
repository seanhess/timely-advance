port module Main exposing (Model, Msg(..), PageModel(..), changeRouteTo, init, main, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Element
import Html exposing (Html, a, b, button, div, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Nimble.Api exposing (Account)
import Page.Accounts as Accounts
import Page.Onboard as Onboard
import Page.Signup as Signup
import Route exposing (Route)
import Url exposing (Url)



-- MODEL -- sub-views? Nested?


type alias Model =
    { key : Nav.Key
    , url : Url
    , page : PageModel
    }


type PageModel
    = NotFound
    | Onboard Onboard.Model
    | Signup Signup.Model
    | Accounts Accounts.Model


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( page, cmd ) =
            changeRouteTo key (Route.fromUrl url)
    in
    ( { key = key
      , url = url
      , page = page
      }
    , cmd
    )



-- UPDATE


type Msg
    = Ignored
      -- | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotOnboardMsg Onboard.Msg
    | GotSignupMsg Signup.Msg
    | GotAccountsMsg Accounts.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- Debug.log (Debug.toString msg) <|
    case ( msg, model.page ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ChangedUrl url, _ ) ->
            let
                ( page, cmd ) =
                    changeRouteTo model.key (Route.fromUrl url)
            in
            ( { model | page = page, url = url }, cmd )

        -- this allows us to intercept the url, I think
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( GotOnboardMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotSignupMsg sub, Signup m ) ->
            Signup.update sub m
                |> runUpdates (always Cmd.none) Signup GotSignupMsg model

        -- |> updateWith Signup GotSignupMsg model
        ( GotAccountsMsg acc, Accounts m ) ->
            Accounts.update acc m
                |> updateWith Accounts GotAccountsMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )



-- when we have Updates in this format:


type alias Event a =
    Maybe a


type alias Updates model msg event =
    ( model, Cmd msg, Event event )


runUpdates : (event -> Cmd Msg) -> (model -> PageModel) -> (msg -> Msg) -> Model -> ( model, Cmd msg, Event event ) -> ( Model, Cmd Msg )
runUpdates eventToMessage toModel toMsg model ( subModel, subCmd, subEvent ) =
    ( { model | page = toModel subModel }
    , Cmd.batch
        [ Cmd.map toMsg subCmd
        , Maybe.withDefault Cmd.none (Maybe.map eventToMessage subEvent)
        ]
    )



-- TODO change everything to use this and just apply it


updateWith : (subModel -> PageModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | page = toModel subModel }
    , Cmd.map toMsg subCmd
    )


changeRouteTo : Nav.Key -> Maybe Route -> ( PageModel, Cmd Msg )
changeRouteTo key maybeRoute =
    case maybeRoute of
        Nothing ->
            ( NotFound, Cmd.none )

        Just Route.Onboard ->
            ( Onboard Onboard.init, Cmd.none )

        Just Route.Signup ->
            ( Signup (Signup.init key), Cmd.none )

        Just Route.Accounts ->
            let
                ( mod, cmd ) =
                    Accounts.init
            in
            ( Accounts mod, Cmd.map GotAccountsMsg cmd )


view : Model -> Browser.Document Msg
view model =
    let
        viewLink path =
            li [] [ a [ href path ] [ text path ] ]

        pageView page =
            case page of
                NotFound ->
                    Element.text "Not Found"

                Onboard o ->
                    Element.map GotOnboardMsg <| Onboard.view o

                Signup s ->
                    Element.map GotSignupMsg <| Signup.view s

                Accounts s ->
                    Element.map GotAccountsMsg <| Accounts.view s
    in
    { title = "TODO: Title"
    , body =
        [ div [] [ Element.layout [] (pageView model.page) ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Onboard mod ->
            Sub.map GotOnboardMsg (Onboard.subscriptions mod)

        Signup mod ->
            Sub.map GotSignupMsg (Signup.subscriptions mod)

        _ ->
            Sub.none


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
