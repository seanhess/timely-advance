module Page.Account exposing (Model, Msg(..), Page(..), changeRouteTo, init, layout, subscriptions, update)

import Browser.Navigation as Nav
import Element exposing (Element, centerY, fill, height, htmlAttribute, padding, width)
import Element.Background as Background
import Html exposing (Html)
import Html.Attributes exposing (style)
import Page.Account.Bills as Bills
import Page.Account.Breakdown as Breakdown
import Page.Account.Budget as Budget
import Page.Account.Home as Home
import Page.Account.Spending as Spending
import Page.Advance as Advance
import Platform.Updates exposing (Updates, command, initWith, updates)
import Process
import Route exposing (Route)
import Task
import Timely.Api
import Timely.Components as Components
import Timely.Style as Style
import Timely.Types exposing (Id, idValue)
import Timely.Types.Account exposing (AccountId)
import Url exposing (Url)


type alias Model =
    { page : Page
    , oldPage : Maybe Page
    , loading : Bool
    }


type Page
    = Home Home.Model
      -- | Breakdown Breakdown.Model
    | Budget Budget.Model
    | Bills Bills.Model
    | Spending Home.Model Spending.Model
    | Advance Advance.Model


type Msg
    = OnHome Home.Msg
      -- | OnBreakdown Breakdown.Msg
    | OnBudget Budget.Msg
    | OnAdvance Advance.Msg
    | OnBills Bills.Msg
    | OnSpending Spending.Msg
    | Loaded ()


init : Id AccountId -> Nav.Key -> Maybe Model -> Route.Account -> ( Model, Cmd Msg )
init i key oldModel route =
    -- I want the change to use my old page if it exists here
    let
        oldPage =
            Maybe.map .page oldModel

        ( page, cmd ) =
            changeRouteTo i key oldPage route
    in
    ( { page = page
      , loading = True
      , oldPage = oldPage
      }
    , Cmd.batch
        [ Task.perform Loaded <| Process.sleep 1, cmd ]
    )


changeRouteTo : Id AccountId -> Nav.Key -> Maybe Page -> Route.Account -> ( Page, Cmd Msg )
changeRouteTo i key oldPage route =
    let
        oldHome op =
            case op of
                Just (Home hm) ->
                    Just hm

                Just (Spending sp _) ->
                    Just sp

                _ ->
                    Nothing
    in
    case route of
        Route.AccountMain ->
            let
                ( hMod, mMsg ) =
                    Home.init key i
            in
            ( Home (oldHome oldPage |> Maybe.withDefault hMod)
            , Cmd.map OnHome mMsg
            )

        Route.Advance adv ->
            -- Check session!
            Advance.init key i adv
                |> initWith Advance OnAdvance

        Route.Bills ->
            Bills.init key i
                |> initWith Bills OnBills

        Route.Budget t bi ->
            Budget.init key i t bi
                |> initWith Budget OnBudget

        Route.Spending ->
            let
                ( hmModel, hmMsg ) =
                    Home.init key i

                ( aModel, aMsg ) =
                    -- use the old budget if it exists
                    Spending.init key i
            in
            ( Spending (oldHome oldPage |> Maybe.withDefault hmModel) aModel
            , Cmd.batch
                [ Cmd.map OnSpending aMsg
                , Cmd.map OnHome hmMsg
                ]
            )



-- maybe I should ALWAYS have an offscreen modal ready to go
-- layout : String -> Model -> Html Msg
-- layout loaded model =
--     Components.layout loaded [] <|
--         view model


layout : String -> Model -> Html Msg
layout loaded model =
    case model.page of
        Spending h a ->
            Components.layout loaded
                [ Element.inFront
                    (modal model.loading (Element.map OnSpending <| Spending.view a))
                ]
                (Element.map OnHome <| Home.view h)

        Home a ->
            case model.oldPage of
                Just (Spending _ s) ->
                    Components.layout loaded
                        [ Element.inFront
                            -- same as normal budget, but direction is reversed
                            (modal (not model.loading) (Element.map OnSpending <| Spending.view s))
                        ]
                        (Element.map OnHome <| Home.view a)

                _ ->
                    Components.layout loaded [] <| Element.map OnHome <| Home.view a

        Bills a ->
            Components.layout loaded [] <| Element.map OnBills <| Bills.view a

        Budget a ->
            Components.layout loaded [] <| Element.map OnBudget <| Budget.view a

        Advance a ->
            Components.layout loaded [] <| Element.map OnAdvance <| Advance.view a



-- TODO transform this color slowly to the fade
-- TODO drop the modal in from the top


modal : Bool -> Element msg -> Element msg
modal offScreen content =
    Element.el
        ([ width fill
         , height fill
         , htmlAttribute (style "transition" "all 0.5s ease")
         ]
            ++ List.map htmlAttribute
                (if offScreen then
                    [ style "background" "transparent"
                    , style "pointer-events" "none"
                    ]

                 else
                    [ style "background" "rgba(0.3, 0.3, 0.3, 0.5)" ]
                )
        )
        (Element.el
            [ padding 10
            , width fill
            , htmlAttribute (style "transition" "all 0.5s ease")
            , htmlAttribute
                (if offScreen then
                    style "transform" "translate3d(0, -600px, 0)"

                 else
                    style "transform" "translate3d(0, 0, 0)"
                )
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

        ( OnBills mg, Bills m ) ->
            Bills.update mg m
                |> runUpdates Bills OnBills model

        ( OnBudget mg, Budget m ) ->
            Budget.update mg m
                |> runUpdates Budget OnBudget model

        -- We have to handle messages from both budget and breakdown
        ( OnSpending set, Spending bm m ) ->
            Spending.update set m
                |> runUpdates (Spending bm) OnSpending model

        ( OnHome mg, Spending hm m ) ->
            Home.update key mg hm
                |> runUpdates (\bm2 -> Spending bm2 m) OnHome model

        -- ( OnBreakdown mg, Breakdown m ) ->
        --     Breakdown.update mg m
        --         |> runUpdates Breakdown OnBreakdown model
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
