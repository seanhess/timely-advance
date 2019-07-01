module Timely.Resource exposing (Resource(..), apply, error, fromResult, map, map2, map3, pure, resource, resource_, toMaybe)

import Element exposing (Element, el, text)
import Http exposing (Error)
import Timely.Components as Components
import Timely.Style as Style


type Resource a
    = Loading
    | Failed Http.Error
    | Ready a


fromResult : Result Error a -> Resource a
fromResult res =
    case res of
        Err e ->
            Failed e

        Ok a ->
            Ready a


toMaybe : Resource a -> Maybe a
toMaybe res =
    case res of
        Ready a ->
            Just a

        _ ->
            Nothing


resource_ : (Error -> Element msg) -> (a -> Element msg) -> Resource a -> Element msg
resource_ errHtml goodHtml res =
    case res of
        Loading ->
            Components.spinner

        Failed e ->
            errHtml e

        Ready a ->
            goodHtml a


error : Http.Error -> Element msg
error e =
    el [ Style.error ] (text "Error")



-- el [ Style.error ] (text <| Debug.toString e)


resource : (a -> Element msg) -> Resource a -> Element msg
resource goodHtml res =
    resource_ error goodHtml res


map : (a -> b) -> Resource a -> Resource b
map f res =
    andThen (Ready << f) res


map2 : (a -> b -> c) -> Resource a -> Resource b -> Resource c
map2 f ra rb =
    case ( ra, rb ) of
        ( Failed e, _ ) ->
            Failed e

        ( _, Failed e ) ->
            Failed e

        ( Ready a, Ready b ) ->
            Ready (f a b)

        _ ->
            Loading


map3 : (a -> b -> c -> d) -> Resource a -> Resource b -> Resource c -> Resource d
map3 f ra rb rc =
    map2 (\( a, b ) c -> f a b c) (map2 (\a b -> ( a, b )) ra rb) rc


andThen : (a -> Resource b) -> Resource a -> Resource b
andThen f ra =
    case ra of
        Loading ->
            Loading

        Failed e ->
            Failed e

        Ready a ->
            f a


apply : Resource b -> Resource (b -> c) -> Resource c
apply rb rf =
    -- ap                :: (Monad m) => m (a -> b) -> m a -> m b
    -- ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
    -- =======
    -- andThen (\b -> andThen (\f -> Ready (f b)) rf) rb
    map2 (\f b -> f b) rf rb


pure : a -> Resource a
pure a =
    Ready a


type A
    = A


type B
    = B


type C
    = C


test : Resource ( A, B, C )
test =
    let
        ra =
            Ready A

        rb =
            Ready B

        rc =
            Ready C
    in
    pure check
        |> apply ra
        |> apply rb
        |> apply rc



-- equivalent to
-- Oh, I want a Resource Element Msg


check : A -> B -> C -> ( A, B, C )
check a b c =
    ( a, b, c )
