module Page.Advance exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Html
import Http
import Json.Encode as Encode
import Platform.Updates exposing (Updates, base, command, set)
import Process
import Route
import Task
import Time
import Timely.Api as Api exposing (Account, AccountId, Advance, AdvanceId, Application, ApprovalResult(..), Id(..), Money, advanceIsActive, formatDate, formatDollars, fromDollars, idValue)
import Timely.Components as Components
import Timely.Resource exposing (Resource(..), resource)
import Timely.Style as Style


type alias Model =
    { key : Nav.Key
    , accountId : Id AccountId
    , advanceId : Id AdvanceId
    , acceptAmount : String
    , advance : Resource Advance
    , account : Resource Account
    , advances : Resource (List Advance)
    , zone : Time.Zone
    }


type Msg
    = OnAdvance (Result Http.Error Advance)
    | OnAccount (Result Http.Error Account)
    | OnAdvances (Result Http.Error (List Advance))
    | OnTimezone Time.Zone
    | Edit String
    | Submit Advance
    | OnAccept (Result Http.Error Advance)


init : Nav.Key -> Id AccountId -> Id AdvanceId -> ( Model, Cmd Msg )
init key accountId advanceId =
    ( { accountId = accountId
      , advanceId = advanceId
      , advance = Loading
      , advances = Loading
      , account = Loading
      , acceptAmount = ""
      , key = key
      , zone = Time.utc
      }
    , Cmd.batch
        [ Api.getAdvance OnAdvance accountId advanceId
        , Api.getAccount OnAccount accountId
        , Api.getAdvances OnAdvances accountId
        , Api.timezone OnTimezone
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    let
        updates =
            base model
    in
    case msg of
        OnAdvance (Err e) ->
            updates |> set { model | advance = Failed e }

        OnAdvance (Ok a) ->
            updates |> set { model | advance = Ready a }

        OnAccount (Err e) ->
            updates |> set { model | account = Failed e }

        OnAccount (Ok a) ->
            updates |> set { model | account = Ready a }

        OnAdvances (Err e) ->
            updates |> set { model | advances = Failed e }

        OnAdvances (Ok a) ->
            updates |> set { model | advances = Ready (List.filter advanceIsActive a) }

        OnTimezone zone ->
            updates |> set { model | zone = zone }

        Edit s ->
            updates |> set { model | acceptAmount = s }

        Submit advance ->
            -- TODO validator for amount, dont' allow submission without it!
            let
                amount =
                    Maybe.withDefault advance.offer <| Maybe.map fromDollars (String.toInt model.acceptAmount)
            in
            updates
                |> set { model | advance = Loading }
                |> command (Api.postAdvanceAccept OnAccept model.accountId model.advanceId amount)

        OnAccept (Err e) ->
            updates |> set { model | advance = Failed e }

        OnAccept (Ok a) ->
            updates |> set { model | advance = Ready a }


view : Model -> Element Msg
view model =
    Element.column Style.formPage
        [ el Style.header (text "Advance")
        , viewStatus model
        ]


viewStatus : Model -> Element Msg
viewStatus model =
    case ( model.account, model.advance, model.advances ) of
        ( Failed _, _, _ ) ->
            viewProblem "Account error"

        ( _, Failed _, _ ) ->
            viewProblem "Advance error"

        ( _, _, Failed _ ) ->
            viewProblem "Advances error"

        ( Ready account, Ready advance, Ready advances ) ->
            case advance.activated of
                Just _ ->
                    viewAccepted model.zone model.accountId advance

                Nothing ->
                    viewForm model account advance advances

        _ ->
            Components.spinner


viewForm : Model -> Account -> Advance -> List Advance -> Element Msg
viewForm model account advance advances =
    Element.column [ spacing 15 ]
        [ Element.el [] (text <| "Credit Used: $" ++ formatDollars (usedCredit advances) ++ " / $" ++ formatDollars account.credit)
        , Input.text [ htmlAttribute (Html.type_ "number") ]
            { text = model.acceptAmount
            , placeholder = Just <| Input.placeholder [] (text <| formatDollars <| advance.offer)
            , onChange = Edit
            , label = Input.labelAbove [ Font.size 14 ] (text "Amount (USD)")
            }
        , Input.button Style.button
            { onPress = Just (Submit advance)
            , label = Element.text "Accept"
            }
        ]


usedCredit : List Advance -> Money
usedCredit advances =
    advances
        |> List.map .amount
        |> List.sum



-- | this should be completely different: show the due date, etc


viewAccepted : Time.Zone -> Id AccountId -> Advance -> Element Msg
viewAccepted zone accountId advance =
    Element.column [ spacing 15 ]
        [ Element.el [] (text "Yay! Your money is on its way")
        , Element.el [] (text <| "Amount: $" ++ formatDollars advance.amount)
        , Element.el [] (text <| "Due: " ++ formatDate zone advance.due)
        , Element.link Style.button
            { url = Route.url <| Route.Account accountId <| Route.AccountMain
            , label = Element.text "My Account"
            }
        ]


viewProblem : String -> Element Msg
viewProblem p =
    el [] (text p)
