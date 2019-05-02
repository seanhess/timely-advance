module Page.Account exposing (Model, Msg(..), Page(..), changeRouteTo, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (Element)
import Page.Account.Breakdown as Breakdown
import Page.Account.Budget as Budget
import Page.Account.Home as Home
import Page.Advance as Advance
import Platform.Updates exposing (Updates, command, initWith, updates)
import Route exposing (Route)
import Timely.Api exposing (AccountId)
import Timely.Types exposing (Id, idValue)
import Url exposing (Url)


type alias Model =
    { page : Page
    }


type Page
    = Home Home.Model
    | Budget Budget.Model
    | Breakdown Breakdown.Model
    | Advance Advance.Model


type Msg
    = OnBreakdown Breakdown.Msg
    | OnHome Home.Msg
    | OnAdvance Advance.Msg
    | OnBudget Budget.Msg


init : Id AccountId -> Nav.Key -> Route.Account -> ( Model, Cmd Msg )
init i key route =
    let
        ( page, cmd ) =
            changeRouteTo i key route
    in
    ( { page = page
      }
    , Cmd.batch
        [ cmd ]
    )


changeRouteTo : Id AccountId -> Nav.Key -> Route.Account -> ( Page, Cmd Msg )
changeRouteTo i key route =
    case route of
        Route.AccountMain ->
            Home.init i
                |> initWith Home OnHome

        Route.Advance adv ->
            -- Check session!
            Advance.init key i adv
                |> initWith Advance OnAdvance

        Route.Budget t b ->
            Budget.init key i t b
                |> initWith Budget OnBudget

        Route.Breakdown ->
            Breakdown.init key i
                |> initWith Breakdown OnBreakdown


view : Model -> Element Msg
view model =
    case model.page of
        Budget a ->
            Element.map OnBudget <| Budget.view a

        Home a ->
            Element.map OnHome <| Home.view a

        Advance a ->
            Element.map OnAdvance <| Advance.view a

        Breakdown m ->
            Element.map OnBreakdown <| Breakdown.view m


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update key msg model =
    case ( msg, model.page ) of
        ( OnHome acc, Home m ) ->
            Home.update key acc m
                |> runUpdates Home OnHome model

        ( OnAdvance adv, Advance m ) ->
            Advance.update adv m
                |> runUpdates Advance OnAdvance model

        ( OnBudget set, Budget m ) ->
            Budget.update set m
                |> runUpdates Budget OnBudget model

        ( OnBreakdown mg, Breakdown m ) ->
            Breakdown.update mg m
                |> runUpdates Breakdown OnBreakdown model

        ( _, _ ) ->
            updates model


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Advance mod ->
            Sub.map OnAdvance (Advance.subscriptions mod)

        Home mod ->
            Sub.map OnHome (Home.subscriptions mod)

        _ ->
            Sub.none


runUpdates : (model -> Page) -> (msg -> Msg) -> Model -> Updates model msg () -> Updates Model Msg ()
runUpdates toPage toMsg model ( subModel, subCmd, _ ) =
    updates { model | page = toPage subModel }
        |> command (Cmd.map toMsg subCmd)
