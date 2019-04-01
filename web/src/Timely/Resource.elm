module Timely.Resource exposing (Resource(..), error, map, map2, map3, resource, resource_)

import Element exposing (Element, el, text)
import Http exposing (Error)
import Timely.Components as Components
import Timely.Style as Style


type Resource a
    = Loading
    | Failed Http.Error
    | Ready a


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
error _ =
    el [ Style.error ] (text "Error")


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
