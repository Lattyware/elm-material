module Material.Attributes exposing
    ( fullWidth
    , label
    , slot
    )

import Html
import Html.Attributes as HtmlA
import Json.Encode as Json


label : String -> Html.Attribute msg
label =
    HtmlA.attribute "label"


slot : String -> Html.Attribute msg
slot =
    HtmlA.attribute "slot"


fullWidth : Html.Attribute msg
fullWidth =
    HtmlA.attribute "fullwidth" ""
