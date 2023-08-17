module Material.ListView exposing
    ( Action(..)
    , IconSize(..)
    , action
    , interactive
    , view
    , viewItem
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Encode as Json
import Material.Attributes as HtmlA


{-| A list.
-}
view : List (Html.Attribute msg) -> List (Html msg) -> Html msg
view attributes children =
    Html.node "mwc-list" attributes children


{-| What happens when you interact with the item.
-}
type Action msg
    = None
    | Link (Html msg -> Html msg)
    | Button (Maybe msg)


type IconSize
    = Icon
    | Medium
    | Large
    | Control
    | Avatar


{-| A list item that is only sometimes enabled.
-}
interactive : msg -> Bool -> Action msg
interactive msg enabled =
    if enabled then
        msg |> Just |> Button

    else
        Nothing |> Button


{-| An action from a maybe, if it is enabled.
-}
action : Maybe msg -> Action msg
action =
    Button


{-| An item within a list.
-}
viewItem : Action msg -> Maybe ( IconSize, Html msg ) -> Maybe (List (Html msg)) -> Maybe (List (Html msg)) -> List (Html msg) -> Html msg
viewItem action_ icon secondary meta children =
    let
        iconSizeToString iconSize =
            case iconSize of
                Icon ->
                    "icon"

                Medium ->
                    "medium"

                Large ->
                    "large"

                Control ->
                    "control"

                Avatar ->
                    "avatar"

        ( optionalAttrs, optionalSlots ) =
            [ icon |> Maybe.map (\( s, i ) -> ( s |> iconSizeToString |> HtmlA.attribute "graphic", Html.span [ HtmlA.slot "graphic" ] [ i ] ))
            , meta |> Maybe.map (\m -> ( True |> Json.bool |> HtmlA.property "hasMeta", Html.span [ HtmlA.slot "meta" ] m ))
            , secondary |> Maybe.map (\s -> ( True |> Json.bool |> HtmlA.property "twoline", Html.span [ HtmlA.slot "secondary" ] s ))
            ]
                |> List.filterMap identity
                |> List.unzip

        ( parent, actionAttr ) =
            case action_ of
                Button (Just msg) ->
                    ( identity, [ msg |> HtmlE.onClick ] )

                Button Nothing ->
                    ( identity, [ HtmlA.disabled True ] )

                Link a ->
                    ( a, [] )

                None ->
                    ( identity, [ True |> Json.bool |> HtmlA.property "noninteractive" ] )

        attrs =
            List.concat [ optionalAttrs, actionAttr ]

        slots =
            List.concat [ optionalSlots, [ Html.span [] children ] ]
    in
    Html.node "mwc-list-item" attrs slots |> parent
