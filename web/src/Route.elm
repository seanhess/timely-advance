module Route exposing (Route(..), fromUrl, href, replaceUrl, routeToString)

-- import Article.Slug as Slug exposing (Slug)
-- import Profile exposing (Profile)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- import Username exposing (Username) ROUTING


type Route
    = Home
    | Onboard



-- | Article Slug
-- | Profile Username
-- | NewArticle
-- | EditArticle Slug


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Onboard (s "onboard")

        -- , Parser.map Profile (s "profile" </> Username.urlParser)
        -- , Parser.map Article (s "article" </> Slug.urlParser)
        -- , Parser.map NewArticle (s "editor")
        -- , Parser.map EditArticle (s "editor" </> Slug.urlParser)
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


woot : String
woot =
    "Hello"



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Onboard ->
                    [ "onboard" ]
    in
    "#/" ++ String.join "/" pieces
