module Route exposing (Route(..), fromUrl, href, replaceUrl, url)

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


type Route
    = Onboard
    | Signup
    | Accounts
    | Account AccountId



-- | Article Slug
-- | Profile Username
-- | NewArticle
-- | EditArticle Slug


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Onboard Parser.top
        , Parser.map Signup (s "signup")
        , Parser.map Accounts (s "accounts")
        , Parser.map Account (s "accounts" </> string)

        -- , Parser.map Profile (s "profile" </> Username.urlParser)
        -- , Parser.map Article (s "article" </> Slug.urlParser)
        -- , Parser.map NewArticle (s "editor")
        -- , Parser.map EditArticle (s "editor" </> Slug.urlParser)
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
                Onboard ->
                    []

                Signup ->
                    [ "signup" ]

                Accounts ->
                    [ "accounts" ]

                Account s ->
                    [ "accounts", s ]
    in
    "#/" ++ String.join "/" pieces
