module Material.LinearProgress exposing
    ( Progress(..)
    , view
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Json.Encode as JsonE


{-| What progress to display.
-}
type
    Progress
    -- An unknown amount of progress.
    = Indeterminate
      -- A float progress from 0 (no progress) to 1 (complete).
    | Progress Float
      -- Complete.
    | Closed


{-| A progress bar.
-}
view : Progress -> List (Html.Attribute msg) -> Html msg
view p attributes =
    Html.node "mwc-linear-progress" (progress p :: attributes) []



{- Private -}


progress : Progress -> Html.Attribute msg
progress p =
    case p of
        Indeterminate ->
            True |> JsonE.bool |> HtmlA.property "indeterminate"

        Progress float ->
            float |> JsonE.float |> HtmlA.property "progress"

        Closed ->
            True |> JsonE.bool |> HtmlA.property "closed"
