module Material.Switch exposing
    ( view
    , viewWithAttrs
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Json


view : Html msg -> Bool -> Maybe (Bool -> msg) -> Html msg
view label checked action =
    viewWithAttrs label checked action []


viewWithAttrs : Html msg -> Bool -> Maybe (Bool -> msg) -> List (Html.Attribute msg) -> Html msg
viewWithAttrs label checked action attrs =
    let
        ( switchAttr, labelAttrs ) =
            case action of
                Just wrap ->
                    ( onCheckNoPropagation wrap, [ checked |> not |> wrap |> HtmlE.onClick ] )

                Nothing ->
                    ( HtmlA.disabled True, [] )
    in
    Html.label (labelAttrs ++ attrs)
        [ Html.node "mwc-switch" [ HtmlA.selected checked, switchAttr ] []
        , label
        ]


onCheckNoPropagation : (Bool -> msg) -> Html.Attribute msg
onCheckNoPropagation msg =
    HtmlE.stopPropagationOn "check" (Json.at [ "target", "checked" ] Json.bool |> Json.map (\c -> ( msg c, True )))
