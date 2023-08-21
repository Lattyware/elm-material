module Material.Menu exposing
    ( Corner(..)
    , MenuCorner(..)
    , State(..)
    , view
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Json.Encode as JsonE


{-| The corner of the anchor element the menu should position itself at.
-}
type Corner
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
    | TopStart
    | TopEnd
    | BottomStart
    | BottomEnd


type MenuCorner
    = Start
    | End


{-| If the menu is visible or not.
-}
type State
    = Open
    | Closed


{-| View a pop-up menu.
-}
view : msg -> State -> Corner -> MenuCorner -> Html msg -> List (Html msg) -> Html msg
view onClose state corner menuCorner anchor menuItems =
    let
        open =
            case state of
                Open ->
                    True

                Closed ->
                    False

        stringCorner =
            case corner of
                TopLeft ->
                    "TOP_LEFT"

                TopRight ->
                    "TOP_RIGHT"

                BottomLeft ->
                    "BOTTOM_LEFT"

                BottomRight ->
                    "BOTTOM_RIGHT"

                TopStart ->
                    "TOP_START"

                TopEnd ->
                    "TOP_END"

                BottomStart ->
                    "BOTTOM_START"

                BottomEnd ->
                    "BOTTOM_END"

        stringMenuCorner =
            case menuCorner of
                Start ->
                    "START"

                End ->
                    "END"
    in
    Html.div [ HtmlA.class "menu-anchor" ]
        [ anchor
        , Html.node "mwc-menu"
            [ True |> JsonE.bool |> HtmlA.property "activatable"
            , open |> JsonE.bool |> HtmlA.property "open"
            , stringCorner |> HtmlA.attribute "corner"
            , stringMenuCorner |> HtmlA.attribute "menuCorner"
            , True |> JsonE.bool |> HtmlA.property "fullwidth"
            , HtmlA.class "menu"
            , onClose |> JsonD.succeed |> HtmlE.on "closing"
            ]
            menuItems
        ]
