module Material.Fab exposing
    ( Type(..)
    , view
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Material.Attributes as HtmlA


type Type
    = Normal
    | Extended String
    | Mini


view : Type -> String -> Html msg -> Maybe msg -> List (Html.Attribute msg) -> Html msg
view type_ title icon action attrs =
    let
        style =
            case type_ of
                Mini ->
                    [ HtmlA.attribute "mini" "" ]

                Extended text ->
                    [ text |> HtmlA.label, HtmlA.attribute "extended" "" ]

                _ ->
                    []

        onClick =
            case action of
                Just msg ->
                    msg |> HtmlE.onClick

                Nothing ->
                    HtmlA.disabled True
    in
    Html.node "mwc-fab"
        (List.concat [ [ title |> HtmlA.title, onClick ], style, attrs ])
        [ Html.span [ HtmlA.slot "icon" ] [ icon ] ]
