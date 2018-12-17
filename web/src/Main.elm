module Main exposing (Model, Msg(..), PageModel(..), changeRouteTo, init, main, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Element
import Html exposing (Html, a, b, button, div, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Nimble.Server exposing (Account)
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
    | GotSignupMsg Signup.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Debug.log (Debug.toString msg) <|
        case ( msg, model.page ) of
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

            ( GotOnboardMsg _, _ ) ->
                ( model, Cmd.none )

            ( GotSignupMsg sub, Signup m ) ->
                Signup.update sub m
                    |> updateWith Signup GotSignupMsg model

            ( _, _ ) ->
                -- Disregard messages that arrived for the wrong page.
                ( model, Cmd.none )



-- Signup.update sub m
--     |> updateWith Signup GotSignupMsg (Signup m)


updateWith : (subModel -> PageModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | page = toModel subModel }
    , Cmd.map toMsg subCmd
    )


changeRouteTo : Maybe Route -> PageModel -> ( PageModel, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( NotFound, Cmd.none )

        Just Route.Onboard ->
            ( Onboard Onboard.init, Cmd.none )

        Just Route.Signup ->
            ( Signup Signup.init, Cmd.none )


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
    in
    { title = "URL Interceptor"
    , body =
        [ text "The URL is: "
        , b [] [ text (Url.toString model.url) ]
        , div [] [ Element.layout [] (pageView model.page) ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Onboard onboard ->
            Sub.map GotOnboardMsg (Onboard.subscriptions onboard)

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
