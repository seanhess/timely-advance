module Page.Account exposing (Model, Msg(..), Page(..), changeRouteTo, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (Element, centerY, fill, height, htmlAttribute, padding, width)
import Element.Background as Background
import Html.Attributes exposing (class)
import Page.Account.Breakdown as Breakdown
import Page.Account.Budget as Budget
import Page.Account.Home as Home
import Page.Advance as Advance
import Platform.Updates exposing (Updates, command, initWith, updates)
import Process
import Route exposing (Route)
import Task
import Timely.Api exposing (AccountId)
import Timely.Style as Style
import Timely.Types exposing (Id, idValue)
import Url exposing (Url)


type alias Model =
    { page : Page
    , loading : Bool
    }


type Page
    = Home Home.Model
    | Breakdown Breakdown.Model
    | Budget Breakdown.Model Budget.Model
    | Advance Advance.Model


type Msg
    = OnHome Home.Msg
    | OnBreakdown Breakdown.Msg
    | OnBudget Budget.Msg
    | OnAdvance Advance.Msg
    | Loaded ()


init : Id AccountId -> Nav.Key -> Route.Account -> ( Model, Cmd Msg )
init i key route =
    -- I want the change to use my old page if it exists here
    let
        ( page, cmd ) =
            changeRouteTo i key route
    in
    ( { page = page
      , loading = True
      }
    , Cmd.batch
        [ Task.perform Loaded <| Process.sleep 1, cmd ]
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
            let
                ( bkModel, bkMsg ) =
                    Breakdown.init key i

                ( bdModel, bdMsg ) =
                    Budget.init key i t b
            in
            ( Budget bkModel bdModel
            , Cmd.batch
                [ Cmd.map OnBudget bdMsg
                , Cmd.map OnBreakdown bkMsg
                ]
            )

        Route.Breakdown ->
            Breakdown.init key i
                |> initWith Breakdown OnBreakdown


view : Model -> Element Msg
view model =
    case model.page of
        Budget b a ->
            Element.column
                [ width fill
                , height fill
                , Element.inFront
                    (modal model.loading (Element.map OnBudget <| Budget.view a))
                ]
                [ Element.map OnBreakdown <| Breakdown.view b
                ]

        -- ,
        -- ]
        Home a ->
            Element.map OnHome <| Home.view a

        Advance a ->
            Element.map OnAdvance <| Advance.view a

        Breakdown m ->
            Element.map OnBreakdown <| Breakdown.view m



-- TODO transform this color slowly to the fade
-- TODO drop the modal in from the top


modal : Bool -> Element msg -> Element msg
modal loading content =
    Element.el
        [ width fill
        , height fill
        , Background.color Style.dim
        , htmlAttribute (class "popup")
        , htmlAttribute (class "background")
        , htmlAttribute
            (if loading then
                class "animate"

             else
                class ""
            )
        ]
        (Element.el
            [ padding 10
            , width fill
            , htmlAttribute (class "content")
            ]
            content
        )


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update key msg model =
    case ( msg, model.page ) of
        ( OnHome acc, Home m ) ->
            Home.update key acc m
                |> runUpdates Home OnHome model

        ( OnAdvance adv, Advance m ) ->
            Advance.update adv m
                |> runUpdates Advance OnAdvance model

        -- We have to handle messages from both budget and breakdown
        ( OnBudget set, Budget bm m ) ->
            Budget.update set m
                |> runUpdates (Budget bm) OnBudget model

        ( OnBreakdown mg, Budget bm m ) ->
            Breakdown.update mg bm
                |> runUpdates (\bm2 -> Budget bm2 m) OnBreakdown model

        ( OnBreakdown mg, Breakdown m ) ->
            Breakdown.update mg m
                |> runUpdates Breakdown OnBreakdown model

        ( Loaded _, _ ) ->
            updates { model | loading = False }

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
