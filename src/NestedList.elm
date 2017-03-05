module NestedList exposing (..)

{-| Module description

## Definition

@docs NestedList

## JSON

### Decoder

@docs decode

### Encoder

@docs encode

## Functions
@docs flatten
-}

import Json.Decode
import Json.Encode


{-| Represents a tree (or nested list)
-}
type NestedList a
    = Element a
    | Nested (List (NestedList a))


{-| Flatten a NestedList to a normal List

    >>> flatten (Nested [ Nested [ Element 1, Element 2, Nested [ Element 3 ] ], Element 4 ])
    [1,2,3,4]
-}
flatten : NestedList a -> List a
flatten list =
    case list of
        Element element ->
            [ element ]

        Nested sublist ->
            List.concatMap flatten sublist


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
    >>> Json.Encode.encode 0 (encode Json.Encode.int (Nested [Nested [Element 1, Element 2], Element 3]))
    "[[1,2],3]"
-}
encode : (a -> Json.Encode.Value) -> NestedList a -> Json.Encode.Value
encode baseEncoder list =
    case list of
        Element element ->
            baseEncoder element

        Nested elements ->
            List.map (encode baseEncoder) elements
                |> Json.Encode.list
