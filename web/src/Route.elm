module Route exposing (Account(..), Admin(..), Onboard(..), Route(..), Settings(..), checkUnauthorized, fromUrl, goAccount, goLanding, href, pushUrl, replaceUrl, url)

-- import Article.Slug as Slug exposing (Slug)
-- import Profile exposing (Profile)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Http exposing (Error(..))
import Timely.Types exposing (Id(..))
import Timely.Types.Account exposing (AccountId)
import Timely.Types.Advance exposing (AdvanceId)
import Timely.Types.Budget exposing (Budget, BudgetId, BudgetType(..))
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- import Username exposing (Username) ROUTING


type Onboard
    = Landing
    | Signup
    | Login
    | Approval (Id AccountId)


type Admin
    = Sudo
    | Customer (Id AccountId)


type Route
    = Init
    | Onboard Onboard
    | Account (Id AccountId) Account
    | Admin Admin
    | Settings (Id AccountId) Settings


type Account
    = AccountMain
    | Advance (Id AdvanceId)
    | Budget BudgetType (Id BudgetId)
    | Bills
    | Spending


type Settings
    = SettingsMain
    | Subscription



-- | Breakdown


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Init Parser.top
        , Parser.map Onboard (s "onboard" </> parserOnboard)
        , Parser.map Admin (s "admin" </> parserAdmin)
        , Parser.map Account (s "accounts" </> Parser.map Id string </> parserAccount)
        , Parser.map Settings (s "settings" </> Parser.map Id string </> parserSettings)
        ]


parserOnboard : Parser (Onboard -> a) a
parserOnboard =
    oneOf
        [ Parser.map Landing Parser.top
        , Parser.map Signup (s "signup")
        , Parser.map Login (s "login")
        , Parser.map Approval (s "approval" </> Parser.map Id string)
        ]


parserAdmin : Parser (Admin -> a) a
parserAdmin =
    oneOf
        [ Parser.map Sudo Parser.top
        , Parser.map Sudo (s "sudo")
        , Parser.map Customer (s "customers" </> Parser.map Id string)
        ]


parserAccount : Parser (Account -> a) a
parserAccount =
    oneOf
        [ Parser.map Advance (s "advances" </> Parser.map Id string)
        , Parser.map AccountMain Parser.top
        , Parser.map (Budget Income) (s "paychecks" </> Parser.map Id string)
        , Parser.map (Budget Expense) (s "bills" </> Parser.map Id string)
        , Parser.map Bills (s "bills")
        , Parser.map Spending (s "spending")

        -- , Parser.map Breakdown (s "breakdown")
        ]


parserSettings : Parser (Settings -> a) a
parserSettings =
    oneOf
        [ Parser.map Subscription (s "subscription")
        , Parser.map SettingsMain Parser.top
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (url targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (url route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (url route)


fromUrl : Url -> Maybe Route
fromUrl u =
    { u | path = Maybe.withDefault "" u.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


url : Route -> String
url page =
    let
        pieces =
            case page of
                Onboard Landing ->
                    [ "onboard" ]

                Onboard Signup ->
                    [ "onboard", "signup" ]

                Onboard Login ->
                    [ "onboard", "login" ]

                Onboard (Approval (Id s)) ->
                    [ "onboard", "approval", s ]

                Admin Sudo ->
                    [ "admin", "sudo" ]

                Admin (Customer (Id s)) ->
                    [ "admin", "customers", s ]

                Account (Id s) AccountMain ->
                    [ "accounts", s ]

                Account (Id s) (Budget Income (Id b)) ->
                    [ "accounts", s, "paychecks", b ]

                Account (Id s) (Budget Expense (Id b)) ->
                    [ "accounts", s, "bills", b ]

                Account (Id s) Bills ->
                    [ "accounts", s, "bills" ]

                Account (Id s) Spending ->
                    [ "accounts", s, "spending" ]

                -- Account (Id s) Breakdown ->
                --     [ "accounts", s, "breakdown" ]
                Account (Id s) (Advance (Id adv)) ->
                    [ "accounts", s, "advances", adv ]

                Settings (Id a) SettingsMain ->
                    [ "settings", a ]

                Settings (Id a) Subscription ->
                    [ "settings", a, "subscription" ]

                Init ->
                    []
    in
    "#/" ++ String.join "/" pieces


goLanding : Nav.Key -> Cmd msg
goLanding key =
    Nav.pushUrl key (url (Onboard Landing))


goAccount : Nav.Key -> Id AccountId -> Cmd msg
goAccount key accountId =
    pushUrl key <| Account accountId AccountMain


checkUnauthorized : Nav.Key -> Result Http.Error a -> Cmd msg
checkUnauthorized key res =
    case res of
        Err (BadStatus 401) ->
            goLanding key

        _ ->
            Cmd.none
