module Page.Advance exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Html
import Http
import Json.Encode as Encode
import Platform.Updates exposing (Updates, command, set, updates)
import Process
import Route
import Task
import Time
import Timely.Api as Api exposing (Account, AccountId, Advance, AdvanceId, Application, ApprovalResult(..), Id(..), advanceIsActive, idValue)
import Timely.Components as Components
import Timely.Resource as Resource exposing (Resource(..), resource)
import Timely.Style as Style
import Timely.Types.Date as Date exposing (formatDate)
import Timely.Types.Money exposing (Money, formatDollars, fromCents, fromDollars, toCents)
import Validate exposing (Validator)


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
    | OnBack
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
        , Date.timezone OnTimezone
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    case msg of
        OnAdvance ra ->
            updates { model | advance = Resource.fromResult ra }

        OnAccount ra ->
            updates { model | account = Resource.fromResult ra }

        OnAdvances ra ->
            updates { model | advances = Resource.fromResult ra }

        OnTimezone zone ->
            updates { model | zone = zone }

        Edit s ->
            updates { model | acceptAmount = s }

        Submit advance ->
            let
                amount =
                    advanceAmount advance.offer model.acceptAmount
            in
            updates { model | advance = Loading }
                |> command (Api.postAdvanceAccept OnAccept model.accountId model.advanceId amount)

        OnAccept ra ->
            updates { model | advance = Resource.fromResult ra }

        OnBack ->
            updates model
                |> command (Route.pushUrl model.key <| Route.Account model.accountId Route.AccountMain)


view : Model -> Element Msg
view model =
    column Style.page
        [ column Style.info
            [ row [ spacing 15 ]
                [ Components.back OnBack
                , el Style.header (text "Advance")
                ]
            ]
        , column Style.section [ viewStatus model ]
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
            case ( advance.activated, advance.collected ) of
                ( Just _, Nothing ) ->
                    viewAccepted model.zone model.accountId advance

                ( _, Just c ) ->
                    viewCollected model.zone model.accountId advance c

                _ ->
                    viewForm model account advance advances

        _ ->
            Components.spinner


viewForm : Model -> Account -> Advance -> List Advance -> Element Msg
viewForm model account advance advances =
    let
        valid =
            Validate.validate (amountValidator advance account advances) model.acceptAmount
    in
    Element.column [ spacing 15, width fill ]
        [ paragraph [] [ text "We predict that you will need some money before your next paycheck. How much would you like?" ]
        , Element.el [] (text <| "Credit Used: $" ++ formatDollars (Api.usedCredit advances) ++ " / $" ++ formatDollars account.credit)
        , Element.el [] (text <| "Suggested: $" ++ formatDollars advance.offer)
        , Input.text [ htmlAttribute (Html.type_ "number") ]
            { text = model.acceptAmount
            , placeholder = Just <| Input.placeholder [] (text <| formatDollars <| advance.offer)
            , onChange = Edit
            , label = Input.labelAbove [ Font.size 14 ] (text "Amount (USD)")
            }
        , case valid of
            Err errs ->
                viewInvalids errs

            Ok amount ->
                viewValid advance
        ]


viewValid : Advance -> Element Msg
viewValid advance =
    Input.button (Style.button Style.success)
        { onPress = Just (Submit advance)
        , label = Element.text "Accept"
        }


viewInvalids : List Invalid -> Element Msg
viewInvalids err =
    Element.column [ spacing 10 ]
        [ Input.button (Style.button Style.primary)
            { onPress = Nothing
            , label = Element.text "Accept"
            }
        , Element.column [ spacing 10 ] (List.map viewInvalid err)
        ]


viewInvalid : Invalid -> Element Msg
viewInvalid inv =
    Element.el [ Style.error ]
        (case inv of
            NoCredit ->
                text "Not enough credit"

            BadAmount _ ->
                text "Invalid amount"
        )



-- | this should be completely different: show the due date, etc


viewAccepted : Time.Zone -> Id AccountId -> Advance -> Element Msg
viewAccepted zone accountId advance =
    Element.column [ spacing 15, width fill ]
        [ Element.el [] (text "Yay! Your money is on its way")
        , Element.el [] (text <| "Amount: $" ++ formatDollars advance.amount)
        , Element.el [] (text <| "Due: " ++ formatDate zone advance.due)
        , Element.link (Style.button Style.secondary)
            { url = Route.url <| Route.Account accountId <| Route.AccountMain
            , label = Element.text "My Account"
            }
        ]


viewCollected : Time.Zone -> Id AccountId -> Advance -> Time.Posix -> Element Msg
viewCollected zone accountId advance collected =
    Element.column [ spacing 15, width fill ]
        [ Element.el [] (text "All paid off!")
        , Element.el [] (text <| "Amount: $" ++ formatDollars advance.amount)
        , Element.el [] (text <| "Paid: " ++ formatDate zone collected)
        , Element.link (Style.button Style.secondary)
            { url = Route.url <| Route.Account accountId <| Route.AccountMain
            , label = Element.text "My Account"
            }
        ]


viewProblem : String -> Element Msg
viewProblem p =
    el [] (text p)


type Invalid
    = NoCredit
    | BadAmount String


amountValidator : Advance -> Account -> List Advance -> Validator Invalid String
amountValidator advance account advances =
    let
        intOrEmpty s =
            case s of
                "" ->
                    "1"

                x ->
                    x

        moreThan0 s =
            case String.toInt s of
                Nothing ->
                    True

                Just x ->
                    x > 0
    in
    Validate.all
        [ Validate.ifNotInt intOrEmpty BadAmount
        , Validate.ifFalse (isEnoughCredit advance account advances) NoCredit
        , Validate.ifFalse moreThan0 (BadAmount "Greater than 0")
        ]


isEnoughCredit : Advance -> Account -> List Advance -> String -> Bool
isEnoughCredit advance account advances value =
    let
        remaining =
            toCents account.credit - toCents (Api.usedCredit advances)
    in
    toCents (advanceAmount advance.offer value) <= remaining


advanceAmount : Money -> String -> Money
advanceAmount offer value =
    Maybe.withDefault offer <| Maybe.map fromCents (String.toInt value)
