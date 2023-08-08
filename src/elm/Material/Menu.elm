module Material.Menu exposing
    ( Corner(..)
    , MenuCorner(..)
    , State(..)
    , view
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode
import Json.Encode as Json


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
            [ True |> Json.bool |> HtmlA.property "activatable"
            , open |> Json.bool |> HtmlA.property "open"
            , stringCorner |> HtmlA.attribute "corner"
            , stringMenuCorner |> HtmlA.attribute "menuCorner"
            , True |> Json.bool |> HtmlA.property "fullwidth"
            , HtmlA.class "menu"
            , onClose |> Json.Decode.succeed |> HtmlE.on "closing"
            ]
            menuItems
        ]
