module Route exposing (AccountId, Onboard(..), Route(..), fromUrl, href, parser, parserOnboard, replaceUrl, url)

-- import Article.Slug as Slug exposing (Slug)
-- import Profile exposing (Profile)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- import Username exposing (Username) ROUTING


type alias AccountId =
    String


type Onboard
    = Landing
    | Signup
    | Phone
    | Approval AccountId


type Route
    = Onboard Onboard
    | Accounts
    | Account AccountId



-- | Article Slug
-- | Profile Username
-- | NewArticle
-- | EditArticle Slug


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Onboard (s "onboard" </> parserOnboard)
        , Parser.map Accounts (s "accounts")
        , Parser.map Account (s "accounts" </> string)

        -- , Parser.map Profile (s "profile" </> Username.urlParser)
        -- , Parser.map Article (s "article" </> Slug.urlParser)
        -- , Parser.map NewArticle (s "editor")
        -- , Parser.map EditArticle (s "editor" </> Slug.urlParser)
        ]


parserOnboard : Parser (Onboard -> a) a
parserOnboard =
    oneOf
        [ Parser.map Landing Parser.top
        , Parser.map Signup (s "signup")
        , Parser.map Phone (s "phone")
        , Parser.map Approval (s "approval" </> string)
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
                    []

                Onboard Signup ->
                    [ "onboard", "signup" ]

                Onboard Phone ->
                    [ "onboard", "phone" ]

                Onboard (Approval s) ->
                    [ "onboard", "approval", s ]

                Accounts ->
                    [ "accounts" ]

                Account s ->
                    [ "accounts", s ]
    in
    "#/" ++ String.join "/" pieces
