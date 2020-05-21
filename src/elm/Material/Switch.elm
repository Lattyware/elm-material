module Material.Switch exposing (view)

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Json


view : Html msg -> Bool -> Maybe (Bool -> msg) -> Html msg
view label checked action =
    let
        ( switchAttr, labelAttrs ) =
            case action of
                Just wrap ->
                    ( onCheckNoPropagation wrap, [ checked |> not |> wrap |> HtmlE.onClick ] )

                Nothing ->
                    ( HtmlA.disabled True, [] )
    in
    Html.label labelAttrs
        [ Html.node "mwc-switch" [ HtmlA.checked checked, switchAttr ] []
        , label
        ]


onCheckNoPropagation : (Bool -> msg) -> Html.Attribute msg
onCheckNoPropagation msg =
    HtmlE.stopPropagationOn "check" (Json.at [ "target", "checked" ] Json.bool |> Json.map (\c -> ( msg c, True )))
