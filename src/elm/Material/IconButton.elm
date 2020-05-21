module Material.IconButton exposing (view)

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE


{-| View a button that displays as a simple icon.
-}
view : Html msg -> String -> Maybe msg -> Html msg
view icon title action =
    let
        actionAttr =
            case action of
                Just msg ->
                    msg |> HtmlE.onClick

                Nothing ->
                    HtmlA.disabled True
    in
    Html.node "mwc-icon-button" [ title |> HtmlA.title, actionAttr ] [ icon ]
