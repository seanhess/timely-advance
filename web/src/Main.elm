port module Main exposing (Model, Msg(..), PageModel(..), changeRouteTo, init, main, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Element
import Html exposing (Html, a, b, button, div, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page.Account as Account
import Page.Accounts as Accounts
import Page.Onboard.Approval as Approval
import Page.Onboard.Landing as Landing
import Page.Onboard.Signup as Signup
import Route exposing (Route)
import Timely.Api exposing (Account)
import Url exposing (Url)



-- MODEL -- sub-views? Nested?


type alias Model =
    { key : Nav.Key
    , url : Url
    , page : PageModel
    }


type PageModel
    = NotFound
    | Landing Landing.Model
    | Signup Signup.Model
    | Approval Approval.Model
    | Accounts Accounts.Model
    | Account Account.Model


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
    | OnLanding Landing.Msg
    | OnSignup Signup.Msg
    | OnApproval Approval.Msg
    | OnAccounts Accounts.Msg
    | OnAccount Account.Msg


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

        ( OnSignup sub, Signup m ) ->
            Signup.update sub m
                |> runUpdates (always Cmd.none) Signup OnSignup model

        ( OnApproval app, Approval m ) ->
            Approval.update app m
                |> runUpdates (always Cmd.none) Approval OnApproval model

        -- |> updateWith Signup GotSignupMsg model
        ( OnAccounts acc, Accounts m ) ->
            Accounts.update acc m
                |> updateWith Accounts OnAccounts model

        ( OnAccount acc, Account m ) ->
            Account.update acc m
                |> updateWith Account OnAccount model

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
            ( Signup (Signup.init key), Cmd.none )

        Just (Route.Onboard (Route.Approval i)) ->
            let
                ( mod, cmd ) =
                    Approval.init key i
            in
            ( Approval mod, Cmd.map OnApproval cmd )

        Just Route.Accounts ->
            let
                ( mod, cmd ) =
                    Accounts.init
            in
            ( Accounts mod, Cmd.map OnAccounts cmd )

        Just (Route.Account i) ->
            let
                ( mod, cmd ) =
                    Account.init i
            in
            ( Account mod, Cmd.map OnAccount cmd )


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

                Accounts s ->
                    Element.map OnAccounts <| Accounts.view s

                Account a ->
                    Element.map OnAccount <| Account.view a
    in
    { title = "TODO: Title"
    , body =
        [ div [] [ Element.layout [] (pageView model.page) ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Signup mod ->
            Sub.map OnSignup (Signup.subscriptions mod)

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
