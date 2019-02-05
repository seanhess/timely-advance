module Route exposing (Account(..), Onboard(..), Route(..), fromUrl, href, replaceUrl, url)

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


type Route
    = Onboard Onboard
    | Accounts
    | Account (Id AccountId) Account
    | Init


type Account
    = AccountMain
    | Advance (Id AdvanceId)



-- | Article Slug
-- | Profile Username
-- | NewArticle
-- | EditArticle Slug


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Init Parser.top
        , Parser.map Onboard (s "onboard" </> parserOnboard)
        , Parser.map Accounts (s "accounts")
        , Parser.map Account (s "accounts" </> Parser.map Id string </> parserAccount)
        ]


parserOnboard : Parser (Onboard -> a) a
parserOnboard =
    oneOf
        [ Parser.map Landing Parser.top
        , Parser.map Signup (s "signup")
        , Parser.map Login (s "login")
        , Parser.map Approval (s "approval" </> Parser.map Id string)
        ]


parserAccount : Parser (Account -> a) a
parserAccount =
    oneOf
        [ Parser.map Advance (s "advance" </> Parser.map Id string)
        , Parser.map AccountMain Parser.top
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (url targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (url route)


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

                Accounts ->
                    [ "accounts" ]

                Account (Id s) AccountMain ->
                    [ "accounts", s ]

                Account (Id s) (Advance (Id adv)) ->
                    [ "accounts", s, "advance", adv ]

                Init ->
                    []
    in
    "#/" ++ String.join "/" pieces
