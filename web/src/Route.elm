module Route exposing (Account(..), Admin(..), Onboard(..), Route(..), fromUrl, href, pushUrl, replaceUrl, url)

-- import Article.Slug as Slug exposing (Slug)
-- import Profile exposing (Profile)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Timely.Api as Api exposing (Account, AccountId, AdvanceId, Id(..))
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- import Username exposing (Username) ROUTING


type Onboard
    = Landing
    | Signup
    | Login
    | Approval (Id AccountId)
    | Budget (Id AccountId)


type Admin
    = Sudo
    | Customer (Id AccountId)


type Route
    = Init
    | Onboard Onboard
    | Account (Id AccountId) Account
    | Admin Admin


type Account
    = AccountMain
    | Advance (Id AdvanceId)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Init Parser.top
        , Parser.map Onboard (s "onboard" </> parserOnboard)
        , Parser.map Admin (s "admin" </> parserAdmin)
        , Parser.map Account (s "accounts" </> Parser.map Id string </> parserAccount)
        ]


parserOnboard : Parser (Onboard -> a) a
parserOnboard =
    oneOf
        [ Parser.map Landing Parser.top
        , Parser.map Signup (s "signup")
        , Parser.map Login (s "login")
        , Parser.map Approval (s "approval" </> Parser.map Id string)
        , Parser.map Budget (s "budget" </> Parser.map Id string)
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
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
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

                Onboard (Budget (Id s)) ->
                    [ "onboard", "budget", s ]

                Admin Sudo ->
                    [ "admin", "sudo" ]

                Admin (Customer (Id s)) ->
                    [ "admin", "customers", s ]

                Account (Id s) AccountMain ->
                    [ "accounts", s ]

                Account (Id s) (Advance (Id adv)) ->
                    [ "accounts", s, "advances", adv ]

                Init ->
                    []
    in
    "#/" ++ String.join "/" pieces
