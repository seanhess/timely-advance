module Main exposing (Model, Msg(..), PageModel(..), changeRouteTo, init, main, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, b, button, div, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Page.Home as Home
import Page.Onboard as Onboard
import Route exposing (Route, routeToString)
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
    | Home Home.Model


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( page, cmd ) =
            changeRouteTo (Route.fromUrl url) NotFound
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
    | GotHomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Debug.log (Debug.toString msg) <|
        case ( msg, model ) of
            ( Ignored, _ ) ->
                ( model, Cmd.none )

            ( ChangedUrl url, _ ) ->
                let
                    ( page, cmd ) =
                        changeRouteTo (Route.fromUrl url) model.page
                in
                ( { model | page = page, url = url }, cmd )

            -- this allows us to intercept the url, I think
            ( ClickedLink urlRequest, _ ) ->
                case urlRequest of
                    Browser.Internal url ->
                        ( model, Nav.pushUrl model.key (Url.toString url) )

                    Browser.External href ->
                        ( model, Nav.load href )

            ( GotHomeMsg _, _ ) ->
                ( model, Cmd.none )

            ( GotOnboardMsg _, _ ) ->
                ( model, Cmd.none )


changeRouteTo : Maybe Route -> PageModel -> ( PageModel, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( NotFound, Cmd.none )

        Just Route.Home ->
            ( Home Home.init, Cmd.none )

        Just Route.Onboard ->
            ( Onboard Onboard.init, Cmd.none )



-- |> updateWith Home GotHomeMsg model
-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewLink path =
            li [] [ a [ href path ] [ text path ] ]

        pageView page =
            case page of
                NotFound ->
                    div [] [ text "Not found" ]

                Onboard o ->
                    Html.map GotOnboardMsg <| Onboard.view o

                Home h ->
                    Html.map GotHomeMsg <| Home.view h
    in
    { title = "URL Interceptor"
    , body =
        [ text "The URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ viewLink (routeToString Route.Home)
            , viewLink (routeToString Route.Onboard)
            ]
        , div [] [ pageView model.page ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFound ->
            Sub.none

        Onboard onboard ->
            Sub.map GotOnboardMsg (Onboard.subscriptions onboard)

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)


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
