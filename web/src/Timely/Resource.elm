module Timely.Resource exposing (Resource(..), error, map, resource, resource_)

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
    case res of
        Loading ->
            Loading

        Failed e ->
            Failed e

        Ready a ->
            Ready (f a)