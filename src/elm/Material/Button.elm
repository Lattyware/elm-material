module Material.Button exposing
    ( Density(..)
    , Type(..)
    , view
    , viewWithAttrs
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Material.Attributes as HtmlA


type Density
    = Padded
    | Dense


type Type
    = Standard
    | Outlined
    | Raised
    | Unelevated


view : Type -> Density -> String -> Maybe (Html msg) -> Maybe msg -> Html msg
view type_ density label icon action =
    viewWithAttrs type_ density label icon action []


viewWithAttrs : Type -> Density -> String -> Maybe (Html msg) -> Maybe msg -> List (Html.Attribute msg) -> Html msg
viewWithAttrs type_ density label icon action extraAttrs =
    let
        typeAttr =
            case type_ of
                Standard ->
                    []

                Outlined ->
                    [ HtmlA.attribute "outlined" "" ]

                Raised ->
                    [ HtmlA.attribute "raised" "" ]

                Unelevated ->
                    [ HtmlA.attribute "unelevated" "" ]

        densityAttr =
            case density of
                Padded ->
                    []

                Dense ->
                    [ HtmlA.attribute "dense" "" ]

        actionAttr =
            case action of
                Just msg ->
                    msg |> HtmlE.onClick

                Nothing ->
                    HtmlA.disabled True

        allAttrs =
            List.concat
                [ [ label |> HtmlA.label
                  , label |> HtmlA.title
                  , actionAttr
                  ]
                , typeAttr
                , densityAttr
                , extraAttrs
                ]

        iconSlot i =
            [ Html.span [ HtmlA.slot "icon" ] [ i ] ]
    in
    Html.node "mwc-button" allAttrs (icon |> Maybe.map iconSlot |> Maybe.withDefault [])
