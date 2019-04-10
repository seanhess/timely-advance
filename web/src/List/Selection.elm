module List.Selection exposing (Selection(..), change, fromList, fromValues, get, ifSelected, isItem, map, member, none, replace, set, toList, toValues)

import List


type Selection a
    = Selection (List ( a, Bool ))


map : (List ( a, Bool ) -> List ( a, Bool )) -> Selection a -> Selection a
map f (Selection s) =
    Selection (f s)


fromValues : List a -> Selection a
fromValues xs =
    Selection <| List.map (\a -> ( a, False )) xs


toValues : Selection a -> List a
toValues (Selection s) =
    List.map (\( a, _ ) -> a) s


fromList : List ( a, Bool ) -> Selection a
fromList =
    Selection


toList : Selection a -> List ( a, Bool )
toList (Selection s) =
    s


listModify : (a -> Bool) -> (a -> a) -> List a -> List a
listModify p f xs =
    let
        update a =
            if p a then
                f a

            else
                a
    in
    List.map update xs



-- replaces the inside. Does not modify selection


replace : (a -> Bool) -> a -> Selection a -> Selection a
replace p a (Selection s) =
    Selection <| listModify (\( x, _ ) -> p x) (\( _, b ) -> ( a, b )) s


listReplace : (a -> Bool) -> a -> List a -> List a
listReplace f a =
    listModify f (\_ -> a)


find : (a -> Bool) -> List a -> Maybe a
find p xs =
    List.head <| List.filter p xs


set : a -> Bool -> Selection a -> Selection a
set a b (Selection s) =
    Selection <| listReplace (isItem a) ( a, b ) s


get : Selection a -> List a
get (Selection s) =
    List.filterMap ifSelected s



-- this sets one to true


change : a -> Selection a -> Selection a
change a s =
    none s
        |> set a True


none : Selection a -> Selection a
none (Selection s) =
    Selection <| List.map (\( a, _ ) -> ( a, False )) s


ifSelected : ( a, Bool ) -> Maybe a
ifSelected ( a, b ) =
    if b then
        Just a

    else
        Nothing


isItem : a -> ( a, Bool ) -> Bool
isItem a1 ( a2, _ ) =
    a1 == a2


member : a -> Selection a -> Bool
member a (Selection s) =
    case find (isItem a) s of
        Just ( _, b ) ->
            b

        _ ->
            False
