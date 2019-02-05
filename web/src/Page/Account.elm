module Page.Account exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Debug
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Route
import Timely.Api as Api exposing (Account, AccountId, Advance, BankAccount, BankAccountType(..), Id, formatDollars, idValue)
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style


type alias Model =
    { accountId : Id AccountId
    , account : Resource Account
    , banks : Resource (List BankAccount)
    , advances : Resource (List Advance)
    }


type Msg
    = OnAccount (Result Http.Error Account)
    | OnBanks (Result Http.Error (List BankAccount))
    | OnAdvances (Result Http.Error (List Advance))
    | Logout
    | LogoutDone (Result Http.Error ())


init : Id AccountId -> ( Model, Cmd Msg )
init id =
    ( { accountId = id
      , account = Loading
      , banks = Loading
      , advances = Loading
      }
    , Cmd.batch
        [ Api.getAccount OnAccount id
        , Api.getAccountBanks OnBanks id
        , Api.getAdvances OnAdvances id
        ]
    )


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update nav msg model =
    case msg of
        OnAccount (Err e) ->
            ( { model | account = Failed e }, Cmd.none )

        OnAccount (Ok acc) ->
            ( { model | account = Ready acc }, Cmd.none )

        OnBanks (Err e) ->
            ( { model | banks = Failed e }, Cmd.none )

        OnBanks (Ok bs) ->
            ( { model | banks = Ready bs }, Cmd.none )

        OnAdvances (Err e) ->
            ( { model | advances = Failed e }, Cmd.none )

        OnAdvances (Ok adv) ->
            ( { model | advances = Ready adv }, Cmd.none )

        Logout ->
            ( model, Api.sessionsLogout LogoutDone )

        LogoutDone _ ->
            ( model, Nav.pushUrl nav (Route.url (Route.Onboard Route.Landing)) )


view : Model -> Element Msg
view model =
    Element.column Style.formPage
        [ el Style.header (text "Account")
        , Input.button [] { onPress = Just Logout, label = text "Logout" }
        , resource accountView model.account
        , el Style.header (text "Offer")
        , resource (advancesView model.accountId) <| Resource.map (List.filter isOffer) model.advances
        , el Style.header (text "Advances")
        , resource (advancesView model.accountId) <| Resource.map (List.filter isActive) model.advances
        , el Style.header (text "Banks")
        , resource banksTable model.banks
        ]


accountView : Account -> Element Msg
accountView account =
    Element.row [ spacing 8, padding 4 ]
        [ Element.column []
            [ el [ Font.bold ] (text "First Name")
            , el [] (text account.customer.firstName)
            ]
        , el [] (text "Last Name")
        , el [ Font.bold ] (text "Last Name")
        , el [] (text account.customer.lastName)
        , el [ Font.bold ] (text "Email")
        , el [] (text account.customer.email)
        ]


banksTable : List BankAccount -> Element Msg
banksTable banks =
    Element.table []
        { data = banks
        , columns =
            [ { header = el [ Font.bold ] (Element.text "Name")
              , width = fill
              , view = \b -> Element.text b.name
              }
            , { header = el [ Font.bold ] (Element.text "Type")
              , width = fill
              , view = \b -> Element.text (accountType b.accountType)
              }
            , { header = el [ Font.bold ] (Element.text "Balance")
              , width = fill
              , view = \b -> Element.text (formatDollars b.balance)
              }
            ]
        }


advancesView : Id AccountId -> List Advance -> Element Msg
advancesView accountId advances =
    Element.column [ spacing 10 ] (List.map (advanceView accountId) advances)


advanceView : Id AccountId -> Advance -> Element Msg
advanceView accountId advance =
    Element.row [ spacing 10 ]
        [ link [ Style.link ] { url = Route.url (Route.Account accountId (Route.Advance advance.advanceId)), label = text <| String.left 8 <| idValue advance.advanceId }
        , Element.el [] (text <| "$" ++ formatDollars advance.offer)

        -- , Element.el [] (text <| Debug.toString advance.offered)
        -- , Element.el [] (text <| Debug.toString advance.activated)
        -- , Element.el [] (text <| Debug.toString advance.collected)
        ]


isOffer : Advance -> Bool
isOffer advance =
    case advance.activated of
        Nothing ->
            True

        Just _ ->
            False


isActive : Advance -> Bool
isActive advance =
    case ( advance.activated, advance.collected ) of
        ( Just _, Nothing ) ->
            True

        _ ->
            False


accountType : BankAccountType -> String
accountType t =
    case t of
        Checking ->
            "Checking"

        Savings ->
            "Savings"

        Credit ->
            "Credit"

        Other ->
            "Other"


viewError : String -> Http.Error -> Element Msg
viewError msg e =
    el [ Style.error ] (text <| msg ++ " " ++ Debug.toString e)
