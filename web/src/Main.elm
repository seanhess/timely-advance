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
import Page.Account.Breakdown as Breakdown
import Page.Account.Budget as Budget
import Page.Admin.Customer as Customer
import Page.Admin.Sudo as Sudo
import Page.Advance as Advance
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
    | Customer Customer.Model
    | Breakdown Breakdown.Model


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
    | OnCustomer Customer.Msg
    | OnBreakdown Breakdown.Msg


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

        ( OnAccount acc, Account m ) ->
            Account.update model.key acc m
                |> updateWith Account OnAccount model

        ( OnAdvance adv, Advance m ) ->
            Advance.update adv m
                |> runUpdates (always Cmd.none) Advance OnAdvance model

        ( OnBudget set, Budget m ) ->
            Budget.update set m
                |> runUpdates (always Cmd.none) Budget OnBudget model

        ( OnBreakdown mg, Breakdown m ) ->
            Breakdown.update mg m
                |> runUpdates (always Cmd.none) Breakdown OnBreakdown model

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


changeRouteTo : Nav.Key -> Maybe Route -> ( PageModel, Cmd Msg )
changeRouteTo key maybeRoute =
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

        Just (Route.Account i Route.AccountMain) ->
            Account.init i
                |> initWith Account OnAccount

        Just (Route.Account a (Route.Advance adv)) ->
            -- Check session!
            Advance.init key a adv
                |> initWith Advance OnAdvance

        Just (Route.Account i (Route.Budget t b)) ->
            Budget.init key i t b
                |> initWith Budget OnBudget

        Just (Route.Account i Route.Breakdown) ->
            Breakdown.init key i
                |> initWith Breakdown OnBreakdown

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

                Customer m ->
                    Element.map OnCustomer <| Customer.view m

                Breakdown m ->
                    Element.map OnBreakdown <| Breakdown.view m
    in
    { title = "Timely Advance"
    , body =
        [ Element.layout []
            (Element.column
                [ Element.width Element.fill, Element.height Element.fill ]
                [ pageView model.page
                , Components.version
                ]
            )
        ]
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



-- when we have Updates in this format:


runUpdates : (event -> Cmd Msg) -> (model -> PageModel) -> (msg -> Msg) -> Model -> Updates model msg event -> ( Model, Cmd Msg )
runUpdates eventToMessage toModel toMsg model ( subModel, subCmd, subEvent ) =
    ( { model | page = toModel subModel }
    , Cmd.batch
        [ Cmd.map toMsg subCmd
        , Maybe.withDefault Cmd.none (Maybe.map eventToMessage subEvent)
        ]
    )


updateWith : (subModel -> PageModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | page = toModel subModel }
    , Cmd.map toMsg subCmd
    )


initWith : (subModel -> PageModel) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( PageModel, Cmd Msg )
initWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
