port module Main exposing (Model, Msg(..), Page(..), changeRouteTo, init, main, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Element exposing (fill, height, width)
import Element.Background as Background
import Html exposing (Html, a, b, button, div, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page.Account as Account
import Page.Admin.Customer as Customer
import Page.Admin.Sudo as Sudo
import Page.Init as Init
import Page.Onboard.Approval as Approval
import Page.Onboard.Landing as Landing
import Page.Onboard.Signup as Signup
import Platform.Updates exposing (Updates)
import Route exposing (Route)
import Timely.Api exposing (Account)
import Timely.Components as Components
import Timely.Style as Style
import Url exposing (Url)
import Version



-- MODEL -- sub-views? Nested?


port appInitialized : String -> Cmd msg


type alias Model =
    { key : Nav.Key
    , url : Url
    , page : Page
    }


type Page
    = NotFound
    | Init Init.Model
    | Landing Landing.Model
    | Signup Signup.Model
    | Approval Approval.Model
    | Account Account.Model
    | Sudo Sudo.Model
    | Customer Customer.Model


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( page, cmd ) =
            changeRouteTo Nothing key (Route.fromUrl url)
    in
    ( { key = key
      , url = url
      , page = page
      }
    , Cmd.batch
        [ cmd
        , appInitialized Version.version
        ]
    )


type Msg
    = Ignored
      -- | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | OnInit Init.Msg
    | OnLanding Landing.Msg
    | OnSignup Signup.Msg
    | OnApproval Approval.Msg
    | OnAccount Account.Msg
    | OnSudo Sudo.Msg
    | OnCustomer Customer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- Debug.log (Debug.toString msg) <|
    case ( msg, model.page ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ChangedUrl url, _ ) ->
            let
                ( page, cmd ) =
                    changeRouteTo (Just model.page) model.key (Route.fromUrl url)
            in
            ( { model | page = page, url = url }, cmd )

        -- this allows us to intercept the url, I think
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( OnLanding mg, Landing m ) ->
            Landing.update mg m
                |> updateWith Landing OnLanding model

        ( OnSignup sub, Signup m ) ->
            Signup.update sub m
                |> runUpdates (always Cmd.none) Signup OnSignup model

        ( OnApproval app, Approval m ) ->
            Approval.update app m
                |> runUpdates (always Cmd.none) Approval OnApproval model

        ( OnAccount acc, Account m ) ->
            Account.update model.key acc m
                |> runUpdates (always Cmd.none) Account OnAccount model

        ( OnSudo mg, Sudo m ) ->
            Sudo.update mg m
                |> runUpdates (always Cmd.none) Sudo OnSudo model

        ( OnCustomer mg, Customer m ) ->
            Customer.update model.key mg m
                |> runUpdates (always Cmd.none) Customer OnCustomer model

        ( OnInit ini, Init m ) ->
            Init.update ini m
                |> updateWith Init OnInit model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


changeRouteTo : Maybe Page -> Nav.Key -> Maybe Route -> ( Page, Cmd Msg )
changeRouteTo maybePage key maybeRoute =
    let
        accountPage mp =
            case mp of
                Just (Account am) ->
                    Just am

                _ ->
                    Nothing
    in
    case maybeRoute of
        Nothing ->
            ( NotFound, Cmd.none )

        Just (Route.Onboard Route.Landing) ->
            Landing.init
                |> initWith Landing OnLanding

        Just (Route.Onboard Route.Signup) ->
            Signup.init key Signup.Signup
                |> initWith Signup OnSignup

        Just (Route.Onboard Route.Login) ->
            Signup.init key Signup.Login
                |> initWith Signup OnSignup

        Just (Route.Onboard (Route.Approval i)) ->
            Approval.init key i
                |> initWith Approval OnApproval

        Just (Route.Admin Route.Sudo) ->
            Sudo.init key
                |> initWith Sudo OnSudo

        Just (Route.Admin (Route.Customer i)) ->
            Customer.init i
                |> initWith Customer OnCustomer

        -- if it's account, I want to send the old one
        Just (Route.Account i route) ->
            Account.init i key (accountPage maybePage) route
                |> initWith Account OnAccount

        Just Route.Init ->
            Init.init key
                |> initWith Init OnInit


view : Model -> Browser.Document Msg
view model =
    let
        viewLink path =
            li [] [ a [ href path ] [ text path ] ]

        pageView page =
            case page of
                NotFound ->
                    Element.text "Not Found"

                Landing o ->
                    Element.map OnLanding <| Landing.view o

                Signup s ->
                    Element.map OnSignup <| Signup.view s

                Approval a ->
                    Element.map OnApproval <| Approval.view a

                Account a ->
                    Element.map OnAccount <| Account.view a

                Init m ->
                    Element.map OnInit <| Init.view m

                Sudo m ->
                    Element.map OnSudo <| Sudo.view m

                Customer m ->
                    Element.map OnCustomer <| Customer.view m
    in
    { title = "Timely Advance"
    , body =
        [ Element.layout []
            (Element.column
                [ width fill, height fill, Element.inFront Components.version ]
                [ pageView model.page ]
            )
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Signup mod ->
            Sub.map OnSignup (Signup.subscriptions mod)

        Account mod ->
            Sub.map OnAccount (Account.subscriptions mod)

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



-- when we have Updates in this format:


runUpdates : (event -> Cmd Msg) -> (model -> Page) -> (msg -> Msg) -> Model -> Updates model msg event -> ( Model, Cmd Msg )
runUpdates eventToMessage toModel toMsg model ( subModel, subCmd, subEvent ) =
    ( { model | page = toModel subModel }
    , Cmd.batch
        [ Cmd.map toMsg subCmd
        , Maybe.withDefault Cmd.none (Maybe.map eventToMessage subEvent)
        ]
    )


updateWith : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | page = toModel subModel }
    , Cmd.map toMsg subCmd
    )


initWith : (subModel -> Page) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Page, Cmd Msg )
initWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
