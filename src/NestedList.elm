module NestedList exposing (..)

{-| Module description

## Definition

@docs NestedList

## JSON

### Decoder

### Encoder


## Functions
@docs flatten
-}


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
