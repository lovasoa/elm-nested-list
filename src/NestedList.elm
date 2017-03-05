module NestedList exposing (..)

{-|  Nested lists and associated Json decoders and encoders for Elm. 

## Definition

@docs NestedList

## JSON

### Decoder

@docs decode

### Encoder

@docs encode

## Functions
@docs map, reduce, flatten
-}

import Json.Decode
import Json.Encode


{-| Represents a tree (or nested list)
-}
type NestedList a
    = Element a
    | Nested (List (NestedList a))


{-| Transform a nested list

    >>> map ((+)1) (Nested [Element 1, Nested [Element 2]])
    Nested [Element 2, Nested [Element 3]]
-}
map : (a -> b) -> NestedList a -> NestedList b
map mapper nested =
    case nested of
        Element element ->
            mapper element
                |> Element

        Nested elements ->
            List.map (map mapper) elements
                |> Nested


{-| Reduce a nested list with a reducer that takes a non-nested list of object and returns a single object

    >>> reduce List.sum (Nested [Nested [Element 1, Element 2], Element 3])
    6
-}
reduce : (List b -> b) -> NestedList b -> b
reduce reducer tree =
    case tree of
        Element element ->
            element

        Nested elements ->
            List.map (reduce reducer) elements |> reducer


{-| Flatten a NestedList to a normal List

    >>> flatten (Nested [ Nested [ Element 1, Element 2, Nested [ Element 3 ] ], Element 4 ])
    [1,2,3,4]
-}
flatten : NestedList a -> List a
flatten =
    map List.singleton >> reduce List.concat


{-| Decode a nested list like [[1,2,[3]],4]

    >>> import Json.Decode exposing (decodeString, int, list)
    >>> decodeString (decode int) "[[1,2],3]"
    Ok (Nested [Nested [Element 1, Element 2], Element 3])
    >>> decodeString (decode (list int)) "[[1,2],[3]]"
    Ok (Nested [Element [1, 2], Element [3]])

-}
decode : Json.Decode.Decoder baseType -> Json.Decode.Decoder (NestedList baseType)
decode baseDecoder =
    let
        self =
            (Json.Decode.lazy (\_ -> decode baseDecoder))
    in
        Json.Decode.oneOf
            [ Json.Decode.list self
                |> Json.Decode.map Nested
            , baseDecoder
                |> Json.Decode.map Element
            ]


{-| Encode a nested list in JSON

    >>> import Json.Encode
    >>> let
    ...    nestedList = Nested [Nested [Element 1, Nested [Element 2]], Element 3]
    ... in
    ...    Json.Encode.encode 0 (encode Json.Encode.int nestedList)
    "[[1,[2]],3]"
-}
encode : (a -> Json.Encode.Value) -> NestedList a -> Json.Encode.Value
encode baseEncoder =
    map baseEncoder >> reduce Json.Encode.list
