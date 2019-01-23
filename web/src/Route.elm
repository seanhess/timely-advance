module Route exposing (AccountId, Onboard(..), Route(..), fromUrl, href, parser, parserOnboard, replaceUrl, url)

-- import Article.Slug as Slug exposing (Slug)
-- import Profile exposing (Profile)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Timely.Api exposing (Account, Id(..))
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- import Username exposing (Username) ROUTING


type alias AccountId =
    String


type Onboard
    = Landing
    | Signup
    | Approval (Id Account)


type Route
    = Onboard Onboard
    | Accounts
    | Account (Id Account)
    | Init



-- | Article Slug
-- | Profile Username
-- | NewArticle
-- | EditArticle Slug


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Onboard (s "onboard" </> parserOnboard)
        , Parser.map Accounts (s "accounts")
        , Parser.map Account (s "accounts" </> Parser.map Id string)
        , Parser.map Init Parser.top
        ]


parserOnboard : Parser (Onboard -> a) a
parserOnboard =
    oneOf
        [ Parser.map Landing Parser.top
        , Parser.map Signup (s "signup")
        , Parser.map Approval (s "approval" </> Parser.map Id string)
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

                Onboard (Approval (Id s)) ->
                    [ "onboard", "approval", s ]

                Accounts ->
                    [ "accounts" ]

                Account (Id s) ->
                    [ "accounts", s ]

                Init ->
                    []
    in
    "#/" ++ String.join "/" pieces
