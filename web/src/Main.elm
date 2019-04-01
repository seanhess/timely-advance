port module Main exposing (Model, Msg(..), PageModel(..), changeRouteTo, init, main, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Element
import Element.Background as Background
import Html exposing (Html, a, b, button, div, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page.Account as Account
import Page.Admin.Sudo as Sudo
import Page.Advance as Advance
import Page.Init as Init
import Page.Onboard.Approval as Approval
import Page.Onboard.Budget as Budget
import Page.Onboard.Landing as Landing
import Page.Onboard.Signup as Signup
import Route exposing (Route)
import Timely.Api exposing (Account)
import Timely.Style as Style
import Url exposing (Url)
import Version



-- MODEL -- sub-views? Nested?


port appInitialized : String -> Cmd msg


type alias Model =
    { key : Nav.Key
    , url : Url
    , page : PageModel
    }


type PageModel
    = NotFound
    | Init Init.Model
    | Landing Landing.Model
    | Signup Signup.Model
    | Approval Approval.Model
    | Budget Budget.Model
    | Account Account.Model
    | Advance Advance.Model
    | Sudo Sudo.Model


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
    , Cmd.batch
        [ cmd
        , appInitialized Version.version
        ]
    )



-- UPDATE


type Msg
    = Ignored
      -- | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | OnInit Init.Msg
    | OnLanding Landing.Msg
    | OnSignup Signup.Msg
    | OnApproval Approval.Msg
    | OnBudget Budget.Msg
    | OnAdvance Advance.Msg
    | OnAccount Account.Msg
    | OnSudo Sudo.Msg


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

        ( OnLanding mg, Landing m ) ->
            Landing.update mg m
                |> updateWith Landing OnLanding model

        ( OnBudget mg, Budget m ) ->
            Budget.update mg m
                |> updateWith Budget OnBudget model

        ( OnSignup sub, Signup m ) ->
            Signup.update sub m
                |> runUpdates (always Cmd.none) Signup OnSignup model

        ( OnApproval app, Approval m ) ->
            Approval.update app m
                |> runUpdates (always Cmd.none) Approval OnApproval model

        ( OnAccount acc, Account m ) ->
            Account.update model.key acc m
                |> updateWith Account OnAccount model

        ( OnAdvance adv, Advance m ) ->
            Advance.update adv m
                |> runUpdates (always Cmd.none) Advance OnAdvance model

        ( OnSudo mg, Sudo m ) ->
            Sudo.update mg m
                |> runUpdates (always Cmd.none) Sudo OnSudo model

        ( OnInit ini, Init m ) ->
            Init.update ini m
                |> updateWith Init OnInit model

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

        Just (Route.Onboard Route.Landing) ->
            let
                ( mod, cmd ) =
                    Landing.init
            in
            ( Landing mod, Cmd.map OnLanding cmd )

        Just (Route.Onboard Route.Signup) ->
            let
                ( mod, cmd ) =
                    Signup.init key Signup.Signup
            in
            ( Signup mod, Cmd.map OnSignup cmd )

        Just (Route.Onboard Route.Login) ->
            let
                ( mod, cmd ) =
                    Signup.init key Signup.Login
            in
            ( Signup mod, Cmd.map OnSignup cmd )

        Just (Route.Onboard (Route.Approval i)) ->
            let
                ( mod, cmd ) =
                    Approval.init key i
            in
            ( Approval mod, Cmd.map OnApproval cmd )

        Just (Route.Onboard (Route.Budget i)) ->
            let
                ( mod, cmd ) =
                    Budget.init i
            in
            ( Budget mod, Cmd.map OnBudget cmd )

        Just (Route.Admin Route.Sudo) ->
            let
                ( mod, cmd ) =
                    Sudo.init key
            in
            ( Sudo mod, Cmd.map OnSudo cmd )

        Just (Route.Account i Route.AccountMain) ->
            let
                ( mod, cmd ) =
                    Account.init i
            in
            ( Account mod, Cmd.map OnAccount cmd )

        Just (Route.Account a (Route.Advance adv)) ->
            -- Check session!
            let
                ( m, cmd ) =
                    Advance.init key a adv
            in
            ( Advance m, Cmd.map OnAdvance cmd )

        Just Route.Init ->
            -- Check session!
            let
                ( m, cmd ) =
                    Init.init key
            in
            ( Init m, Cmd.map OnInit cmd )


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

                Budget a ->
                    Element.map OnBudget <| Budget.view a

                Account a ->
                    Element.map OnAccount <| Account.view a

                Advance a ->
                    Element.map OnAdvance <| Advance.view a

                Init m ->
                    Element.map OnInit <| Init.view m

                Sudo m ->
                    Element.map OnSudo <| Sudo.view m
    in
    { title = "TODO: Title"
    , body = [ Element.layout [] (pageView model.page) ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Signup mod ->
            Sub.map OnSignup (Signup.subscriptions mod)

        Advance mod ->
            Sub.map OnAdvance (Advance.subscriptions mod)

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
