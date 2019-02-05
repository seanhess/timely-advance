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
import Timely.Api as Api exposing (Account, AccountId, Advance, AdvanceId, Application, ApprovalResult(..), Id(..), Money(..), formatDollars, fromDollars, idValue)
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
    }


type Msg
    = OnAdvance (Result Http.Error Advance)
    | OnAccount (Result Http.Error Account)
    | Edit String
    | Submit Advance
    | OnAccept (Result Http.Error Advance)


init : Nav.Key -> Id AccountId -> Id AdvanceId -> ( Model, Cmd Msg )
init key accountId advanceId =
    ( { accountId = accountId
      , advanceId = advanceId
      , advance = Loading
      , account = Loading
      , acceptAmount = ""
      , key = key
      }
    , Cmd.batch
        [ Api.getAdvance OnAdvance accountId advanceId
        , Api.getAccount OnAccount accountId
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
            updates
                |> set { model | advance = Failed e }

        OnAdvance (Ok a) ->
            updates
                |> set { model | advance = Ready a }

        OnAccount (Err e) ->
            updates
                |> set { model | account = Failed e }

        OnAccount (Ok a) ->
            updates
                |> set { model | account = Ready a }

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
            updates
                |> set { model | advance = Failed e }

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
    case ( model.account, model.advance ) of
        ( Failed _, _ ) ->
            viewProblem "Account error"

        ( _, Failed _ ) ->
            viewProblem "Advance error"

        ( Ready account, Ready advance ) ->
            case advance.activated of
                Just _ ->
                    viewAccepted model.accountId

                Nothing ->
                    viewForm model account advance

        _ ->
            Components.spinner


viewForm : Model -> Account -> Advance -> Element Msg
viewForm model account advance =
    Element.column [ spacing 15 ]
        [ Element.el [] (text <| "Credit: $" ++ formatDollars account.credit)
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



-- | this should be completely different: show the due date, etc


viewAccepted : Id AccountId -> Element Msg
viewAccepted accountId =
    Element.column [ spacing 15 ]
        [ Element.el [] (text "Yay! Your money is on its way")
        , Element.link Style.button
            { url = Route.url <| Route.Account accountId <| Route.AccountMain
            , label = Element.text "My Account"
            }
        ]


viewProblem : String -> Element Msg
viewProblem p =
    el [] (text p)
