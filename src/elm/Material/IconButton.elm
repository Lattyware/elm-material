module Material.IconButton exposing
    ( IconButton, view
    , icon, filled, filledTonal, outlined
    , link, replacedLink, button, toggle
    , attrs
    )

{-| Use icon buttons to display actions in a compact layout. Icon buttons can
represent opening actions such as opening an overflow menu or search, or
represent binary actions that can be toggled on and off, such as favorite or
bookmark.

Icon buttons can be grouped together or they can stand alone.


# Core

@docs IconButton, view


# Icon Button Variants

Icon buttons are disabled by default, add an action to change this.

@docs icon, filled, filledTonal, outlined


# Actions

@docs link, replacedLink, button, toggle


# Optional Customisation

@docs attrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Material.Util as Util


type Action msg
    = Link String (Maybe String) (Maybe (String -> Maybe String -> msg))
    | Button (Maybe msg)
    | Toggle (Html msg) (Maybe (Bool -> msg)) Bool


type alias Pipeline msg =
    { node : String
    , icon : Html msg
    , action : Action msg
    , description : String
    , attrs : List (Html.Attribute msg)
    }


{-| An icon button definition, which can be turned into HTML with
[`view`](#view).
-}
type IconButton msg
    = IconButton (Pipeline msg)


init : String -> Html msg -> String -> IconButton msg
init node givenIcon description =
    Pipeline node givenIcon (Button Nothing) description [] |> IconButton


{-| A standard icon button.

Takes the icon and a description.

-}
icon : Html msg -> String -> IconButton msg
icon =
    init "md-icon-button"


{-| Filled icon buttons have higher visual impact and are best for high
emphasis actions.

Takes the icon and a description.

-}
filled : Html msg -> String -> IconButton msg
filled =
    init "md-filled-icon-button"


{-| Filled tonal icon buttons are a middle ground between filled and outlined
icon buttons. They're useful in contexts where the button requires slightly
more emphasis than an outline would give, such as a secondary action paired
with a high emphasis action.

Takes the icon and a description.

-}
filledTonal : Html msg -> String -> IconButton msg
filledTonal =
    init "md-filled-tonal-icon-button"


{-| Outlined icon buttons are medium-emphasis buttons. They're useful when an
icon button needs more emphasis than a standard icon button but less than a
filled or filled tonal icon button.

Takes the icon and a description.

-}
outlined : Html msg -> String -> IconButton msg
outlined =
    init "md-outlined-icon-button"


{-| Give the icon button an action as a simple click button.
If the action is Nothing, the icon button is disabled.
-}
button : Maybe msg -> IconButton msg -> IconButton msg
button action (IconButton pipeline) =
    IconButton { pipeline | action = Button action }


{-| Make the icon button a toggle.
Takes the icon to use when selected, the normal icon is used when not selected.
If the action is Nothing, the icon button is disabled.
Finally, takes the current selected state.
-}
toggle : Html msg -> Maybe (Bool -> msg) -> Bool -> IconButton msg -> IconButton msg
toggle selectedIcon action selected (IconButton pipeline) =
    IconButton { pipeline | action = Toggle selectedIcon action selected }


{-| Give the icon button an action as a link.
Links can't be disabled.
Unfortunately Elm link handling in web components doesn't work, so we have to
take a pushUrl message wrapper to handle that case.
Takes the link url, and the target.
-}
link : String -> Maybe String -> IconButton msg -> IconButton msg
link href target (IconButton pipeline) =
    IconButton { pipeline | action = Link href target Nothing }


{-| Give the icon button an action as a link.
Links can't be disabled.
This replaces the normal click action with an Elm one, half button, half link.
Takes the link url, and the target.
-}
replacedLink : (String -> Maybe String -> msg) -> String -> Maybe String -> IconButton msg -> IconButton msg
replacedLink pushUrl href target (IconButton pipeline) =
    IconButton { pipeline | action = Link href target (Just pushUrl) }


{-| Add custom attributes to the icon button.
-}
attrs : List (Html.Attribute msg) -> IconButton msg -> IconButton msg
attrs newAttrs (IconButton pipeline) =
    IconButton { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Turn an [`IconButton` definition](#IconButton) into HTML.
-}
view : IconButton msg -> Html msg
view (IconButton pipeline) =
    let
        ( actionAttrs, actionContents ) =
            case pipeline.action of
                Link pushUrl href target ->
                    ( Util.replacedLink pushUrl href target, [] )

                Toggle selectedIcon action selected ->
                    let
                        getSelected =
                            JsonD.at [ "target", "selected" ] JsonD.bool
                    in
                    ( [ Util.onActionAttr "change" getSelected action
                      , HtmlA.selected selected
                      ]
                    , [ Html.div [ HtmlA.attribute "slot" "selectedIcon" ]
                            [ selectedIcon ]
                      ]
                    )

                Button action ->
                    ( [ Util.actionAttr HtmlE.onClick action ], [] )
    in
    Html.node pipeline.node
        (List.concat
            [ pipeline.attrs
            , [ HtmlA.attribute "aria-label" pipeline.description
              , HtmlA.title pipeline.description
              ]
            , actionAttrs
            ]
        )
        (pipeline.icon :: actionContents)
