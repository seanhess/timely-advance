module Page.Account exposing (Model, Msg(..), Page(..), changeRouteTo, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (Element, centerY, fill, height, htmlAttribute, padding, width)
import Element.Background as Background
import Html.Attributes exposing (style)
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
    , oldPage : Maybe Page
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
        oldBreakdown op =
            case op of
                Just (Breakdown brk) ->
                    Just brk

                Just (Budget brk _) ->
                    Just brk

                _ ->
                    Nothing
    in
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
                    -- use the old budget if it exists
                    Budget.init key i t b
            in
            ( Budget (oldBreakdown oldPage |> Maybe.withDefault bkModel) bdModel
            , Cmd.batch
                [ Cmd.map OnBudget bdMsg
                , Cmd.map OnBreakdown bkMsg
                ]
            )

        Route.Breakdown ->
            let
                ( bkModel, bkMsg ) =
                    Breakdown.init key i
            in
            ( Breakdown (oldBreakdown oldPage |> Maybe.withDefault bkModel)
            , Cmd.map OnBreakdown bkMsg
            )



-- maybe I should ALWAYS have an offscreen modal ready to go


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

        Home a ->
            Element.map OnHome <| Home.view a

        Advance a ->
            Element.map OnAdvance <| Advance.view a

        Breakdown m ->
            case model.oldPage of
                Just (Budget _ a) ->
                    Element.column
                        [ width fill
                        , height fill
                        , Element.inFront
                            -- same as normal budget, but direction is reversed
                            (modal (not model.loading) (Element.map OnBudget <| Budget.view a))
                        ]
                        [ Element.map OnBreakdown <| Breakdown.view m
                        ]

                _ ->
                    Element.map OnBreakdown <| Breakdown.view m



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
