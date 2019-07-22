port module Main exposing (Model, Msg(..), Page(..), changeRouteTo, init, main, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Element exposing (el, fill, height, moveUp, scrollbarY, width)
import Element.Background as Background
import Html exposing (Html, a, b, button, div, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page.Account as Account
import Page.Admin.Customer as Customer
import Page.Admin.Sudo as Sudo
import Page.Admin.Test as Test
import Page.Init as Init
import Page.Onboard.Approval as Approval
import Page.Onboard.Bank as Bank
import Page.Onboard.Landing as Landing
import Page.Onboard.Signup as Signup
import Page.Settings.Main as Settings
import Page.Settings.Subscription as Subscription
import Platform.Updates exposing (Updates)
import Route exposing (Route)
import Timely.Api
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
    , loaded : String
    }


type Page
    = NotFound
    | Init Init.Model
    | Landing Landing.Model
    | Bank Bank.Model
    | Signup Signup.Model
    | Approval Approval.Model
    | Account Account.Model
    | Sudo Sudo.Model
    | Test Test.Model
    | Customer Customer.Model
    | Settings Settings.Model
    | Subscription Subscription.Model


type Msg
    = Ignored
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | OnInit Init.Msg
    | OnLanding Landing.Msg
    | OnSignup Signup.Msg
    | OnBank Bank.Msg
    | OnApproval Approval.Msg
    | OnAccount Account.Msg
    | OnSudo Sudo.Msg
    | OnTest Test.Msg
    | OnCustomer Customer.Msg
    | OnSubscription Subscription.Msg
    | OnSettings Settings.Msg


init : String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init loaded url key =
    let
        ( page, cmd ) =
            changeRouteTo Nothing key (Route.fromUrl url)
    in
    ( { key = key
      , url = url
      , page = page
      , loaded = loaded
      }
    , Cmd.batch
        [ cmd
        , appInitialized Version.version
        ]
    )


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
            Signup.update model.key sub m
                |> runUpdates (always Cmd.none) Signup OnSignup model

        ( OnBank mg, Bank m ) ->
            Bank.update model.key mg m
                |> runUpdates (always Cmd.none) Bank OnBank model

        ( OnApproval app, Approval m ) ->
            Approval.update model.key app m
                |> runUpdates (always Cmd.none) Approval OnApproval model

        ( OnAccount acc, Account m ) ->
            Account.update model.key acc m
                |> runUpdates (always Cmd.none) Account OnAccount model

        ( OnSudo mg, Sudo m ) ->
            Sudo.update mg m
                |> runUpdates (always Cmd.none) Sudo OnSudo model

        ( OnTest mg, Test m ) ->
            Test.update model.key mg m
                |> runUpdates (always Cmd.none) Test OnTest model

        ( OnCustomer mg, Customer m ) ->
            Customer.update model.key mg m
                |> runUpdates (always Cmd.none) Customer OnCustomer model

        ( OnSettings mg, Settings m ) ->
            Settings.update model.key mg m
                |> runUpdates (always Cmd.none) Settings OnSettings model

        ( OnSubscription mg, Subscription m ) ->
            Subscription.update model.key mg m
                |> runUpdates (always Cmd.none) Subscription OnSubscription model

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
            Signup.init Signup.Signup
                |> initWith Signup OnSignup

        Just (Route.Onboard Route.Login) ->
            Signup.init Signup.Login
                |> initWith Signup OnSignup

        Just (Route.Onboard (Route.Approval i)) ->
            Approval.init i
                |> initWith Approval OnApproval

        Just (Route.Onboard (Route.Bank e)) ->
            Bank.init e
                |> initWith Bank OnBank

        Just (Route.Admin Route.Sudo) ->
            Sudo.init key
                |> initWith Sudo OnSudo

        Just (Route.Admin Route.Test) ->
            Test.init
                |> initWith Test OnTest

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

        Just (Route.Settings i Route.SettingsMain) ->
            Settings.init key i
                |> initWith Settings OnSettings

        Just (Route.Settings i Route.Subscription) ->
            Subscription.init key i
                |> initWith Subscription OnSubscription


view : Model -> Browser.Document Msg
view model =
    let
        viewLink path =
            li [] [ a [ href path ] [ text path ] ]

        layout toMsg pageView =
            Components.layout model.loaded [] <| Element.map toMsg <| pageView

        pageLayout page =
            case page of
                NotFound ->
                    Components.layout model.loaded [] <| Element.text "Not Found"

                Landing o ->
                    layout OnLanding <| Landing.view o

                Signup s ->
                    layout OnSignup <| Signup.view s

                Bank s ->
                    layout OnBank <| Bank.view s

                Approval a ->
                    layout OnApproval <| Approval.view a

                Account a ->
                    Html.map OnAccount <| Account.layout model.loaded a

                Settings a ->
                    layout OnSettings <| Settings.view a

                Subscription a ->
                    layout OnSubscription <| Subscription.view a

                Init m ->
                    layout OnInit <| Init.view m

                Sudo m ->
                    layout OnSudo <| Sudo.view m

                Test m ->
                    layout OnTest <| Test.view m

                Customer m ->
                    layout OnCustomer <| Customer.view m
    in
    { title = "Timely Advance"
    , body =
        [ pageLayout model.page ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Signup mod ->
            Sub.map OnSignup (Signup.subscriptions mod)

        Bank mod ->
            Sub.map OnBank (Bank.subscriptions mod)

        Account mod ->
            Sub.map OnAccount (Account.subscriptions mod)

        _ ->
            Sub.none


main : Program String Model Msg
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
