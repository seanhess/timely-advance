module Page.Settings.Subscription exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Region as Region
import Http exposing (Error)
import Platform.Updates exposing (Updates, command, set, updates)
import Route exposing (Onboard(..), Route(..))
import Timely.Api as Api exposing (AccountId)
import Timely.Components as Components
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types exposing (Id(..))
import Timely.Types.Money as Money exposing (formatMoney)
import Timely.Types.Subscription exposing (Subscription, formatLevel)


type alias Model =
    { accountId : Id AccountId
    , current : Resource Subscription
    , subscriptions : Resource (List Subscription)
    , isSaving : Bool
    }


type alias Resources =
    { current : Subscription
    , subscriptions : List Subscription
    }


init : Nav.Key -> Id AccountId -> ( Model, Cmd Msg )
init key accountId =
    ( { accountId = accountId
      , current = Loading
      , subscriptions = Loading
      , isSaving = False
      }
    , Cmd.batch
        [ Api.getAvailableSubscriptions OnSubscriptions
        , Api.getSubscription OnCurrent accountId
        ]
    )


type Msg
    = OnCurrent (Result Error Subscription)
    | OnSubscriptions (Result Error (List Subscription))
    | Save Subscription
    | OnSave (Result Error String)
    | Cancel
    | Ignore (Result Error String)


update : Nav.Key -> Msg -> Model -> Updates Model Msg ()
update nav msg model =
    case msg of
        OnCurrent r ->
            updates { model | current = Resource.fromResult r }

        OnSubscriptions r ->
            updates { model | subscriptions = Resource.fromResult r }

        Save sub ->
            updates { model | isSaving = True }
                |> command (Api.putSubscription OnSave model.accountId sub.level)

        OnSave _ ->
            updates { model | isSaving = False }
                |> command (Api.getSubscription OnCurrent model.accountId)

        Cancel ->
            updates { model | isSaving = True }
                |> command (Api.delSubscription Ignore model.accountId)
                |> command (Api.sessionsLogout Ignore)
                |> command (Route.goLanding nav)

        Ignore _ ->
            updates model


view : Model -> Element Msg
view model =
    column Style.page
        [ column Style.header
            [ row [ spacing 15 ]
                [ Components.backLink (Route.Settings model.accountId Route.SettingsMain)
                , el Style.heading (text "Modify Subscription")
                ]
            ]
        , resource (viewReady model.isSaving) <| Resource.map2 Resources model.current model.subscriptions
        ]


viewReady : Bool -> Resources -> Element Msg
viewReady isSaving res =
    column Style.section
        [ viewCurrent res.current
        , column [ spacing 10, width fill ]
            (List.map (viewOther isSaving res.current) (List.filter (isOther res.current) res.subscriptions))
        , button [ Style.link, Font.size 16 ]
            { onPress = Just Cancel
            , label = text "Cancel Subscription"
            }
        ]


viewCurrent : Subscription -> Element Msg
viewCurrent sub =
    column [ spacing 15, padding 15, width fill, Background.color Style.green ]
        [ el Style.banner (text "Current Level")
        , viewSubDetails [ Font.color Style.white ] sub
        ]


viewOther : Bool -> Subscription -> Subscription -> Element Msg
viewOther isSaving current sub =
    column [ spacing 15, width fill ]
        [ el [ Font.bold ] (text <| formatLevel sub.level ++ " Level")
        , viewSubDetails [] sub
        , if isUpgrade current sub then
            viewUpgrade isSaving current sub

          else
            viewDowngrade isSaving current sub
        ]


viewUpgrade : Bool -> Subscription -> Subscription -> Element Msg
viewUpgrade isSaving current sub =
    column [ spacing 15, width fill ]
        [ Components.loadingButton (Style.button Style.primary)
            { onPress = Save sub
            , label = text "Upgrade Now"
            , isLoading = isSaving
            }
        , paragraph [ Font.size 16, Font.italic ] [ text <| "Upgrading will increase your subscription cost from " ++ formatMoney current.cost ++ " to " ++ formatMoney sub.cost ++ " / month" ]
        ]


viewDowngrade : Bool -> Subscription -> Subscription -> Element Msg
viewDowngrade isSaving current sub =
    column [ spacing 15, width fill ]
        [ Components.loadingButton (Style.button Style.secondary)
            { onPress = Save sub
            , label = text "Downgrade Now"
            , isLoading = isSaving
            }
        , paragraph [ Font.size 16, Font.italic ] [ text <| "Downgrading will decrease your subscription cost from " ++ formatMoney current.cost ++ " to " ++ formatMoney sub.cost ++ " / month" ]
        ]


viewSubDetails : List (Attribute Msg) -> Subscription -> Element Msg
viewSubDetails atts sub =
    column ([ spacing 10, width fill ] ++ atts)
        [ row [ spacing 10, width fill ]
            [ el [ alignLeft ] (text <| formatLevel sub.level)
            , el [ alignRight ] (text <| formatMoney sub.cost ++ " / month")
            ]
        , row [ spacing 10, width fill ]
            [ el [ alignLeft ] (text "Advance Limit")
            , el [ alignRight ] (text <| formatMoney sub.limit)
            ]
        ]



-- Resources --------------------


isUpgrade : Subscription -> Subscription -> Bool
isUpgrade current other =
    Money.toCents other.cost > Money.toCents current.cost


isOther : Subscription -> Subscription -> Bool
isOther current sub =
    not (current == sub)
