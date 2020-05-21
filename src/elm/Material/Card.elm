module Material.Card exposing (view)

import Html exposing (Html)
import Html.Attributes as HtmlA


view : List (Html.Attribute msg) -> List (Html msg) -> Html msg
view attributes =
    Html.div (HtmlA.class "mdc-card" :: attributes)
