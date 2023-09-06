module Material.Switch exposing
    ( Switch, switch, view
    , attrs
    )

{-| Switches toggle the state of an item on or off.


# Core

@docs Switch, switch, view


# Optional Customisation

@docs attrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Json.Decode as JsonD
import Material.Util as Util


type alias Pipeline msg =
    { selected : Bool
    , action : Maybe (Bool -> msg)
    , icons : Bool
    , showOnlySelectedIcon : Bool
    , attrs : List (Html.Attribute msg)
    }


{-| A switch definition, which can be turned into HTML with
[`view`](#view).
-}
type Switch msg
    = Switch (Pipeline msg)


{-| A switch, takes the action (disabled when Nothing), and if the switch is
selected.
-}
switch : Maybe (Bool -> msg) -> Bool -> Switch msg
switch action selected =
    Pipeline selected action False False [] |> Switch


{-| Turn a [`Switch` definition](#Switch) into HTML.
-}
view : Switch msg -> Html msg
view (Switch pipeline) =
    let
        getSelected =
            JsonD.at [ "target", "selected" ] JsonD.bool

        allAttrs =
            [ [ HtmlA.selected pipeline.selected
              , Util.onActionAttr "change" getSelected pipeline.action
              ]
            , pipeline.attrs
            , pipeline.icons |> Util.boolAttr "icons"
            , pipeline.showOnlySelectedIcon
                |> Util.boolAttr "show-only-selected-icon"
            ]
    in
    Html.node "md-switch" (List.concat allAttrs) []


{-| Add custom attributes to the switch.
-}
attrs : List (Html.Attribute msg) -> Switch msg -> Switch msg
attrs newAttrs (Switch pipeline) =
    Switch { pipeline | attrs = List.append pipeline.attrs newAttrs }
