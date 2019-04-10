module Result.Cat exposing (cat, check)

import List


cat : List (Result e a) -> Result e (List a)
cat =
    List.foldl check (Ok [])


check : Result e a -> Result e (List a) -> Result e (List a)
check r rs =
    case ( r, rs ) of
        ( _, Err e ) ->
            Err e

        ( Err e, _ ) ->
            Err e

        ( Ok x, Ok xs ) ->
            Ok (x :: xs)
