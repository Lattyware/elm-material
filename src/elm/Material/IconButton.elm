module Material.IconButton exposing
    ( view
    , viewNoPropagation
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD


{-| View a button that displays as a simple icon.
-}
view : Html msg -> String -> Maybe msg -> Html msg
view =
    viewInternal HtmlE.onClick


viewNoPropagation : Html msg -> String -> Maybe msg -> Html msg
viewNoPropagation =
    viewInternal onClickNoPropagation


viewInternal : (msg -> Html.Attribute msg) -> Html msg -> String -> Maybe msg -> Html msg
viewInternal onClick icon title action =
    let
        actionAttr =
            case action of
                Just msg ->
                    msg |> onClick

                Nothing ->
                    HtmlA.disabled True
    in
    Html.node "mwc-icon-button" [ title |> HtmlA.title, actionAttr ] [ icon ]


onClickNoPropagation : msg -> Html.Attribute msg
onClickNoPropagation msg =
    HtmlE.stopPropagationOn "click" (JsonD.succeed ( msg, True ))
