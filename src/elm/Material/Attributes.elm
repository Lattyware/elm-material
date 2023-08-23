module Material.Attributes exposing
    ( charCounter
    , fullWidth
    , label
    , outlined
    , slot
    )

import Html
import Html.Attributes as HtmlA


label : String -> Html.Attribute msg
label =
    HtmlA.attribute "label"


slot : String -> Html.Attribute msg
slot =
    HtmlA.attribute "slot"


fullWidth : Html.Attribute msg
fullWidth =
    HtmlA.attribute "fullwidth" ""


outlined : Html.Attribute msg
outlined =
    HtmlA.attribute "outlined" ""


charCounter : Html.Attribute msg
charCounter =
    HtmlA.attribute "charCounter" ""
