module Material.Chips exposing (set)

import Html exposing (Html)


{-| View a set of chips.
-}
set : List (Html.Attribute msg) -> List (Html msg) -> Html msg
set =
    Html.node "md-chip-set"
