module Material.Chips.Suggestion exposing
    ( Suggestion, chip, view
    , button, link, replacedLink
    , icon, elevated, attrs
    )

{-| Suggestion chips help narrow a user’s intent by presenting dynamically
generated suggestions, such as possible responses or search filters.


# Core

@docs Suggestion, chip, view


# Actions

@docs button, link, replacedLink


# Optional Customisation

@docs icon, elevated, attrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Material.Util as Util
import Material.Util.Action as Action exposing (Action)


type alias Pipeline msg =
    { label : String
    , icon : Maybe (List (Html msg))
    , elevated : Bool
    , action : Action msg
    , attrs : List (Html.Attribute msg)
    }


{-| An Suggestion chip.
-}
type Suggestion msg
    = Suggestion (Pipeline msg)


{-| An Suggestion chip, taking its label.
-}
chip : String -> Suggestion msg
chip label =
    Pipeline
        label
        Nothing
        False
        (Action.Button Nothing)
        []
        |> Suggestion


{-| Give the chip an action as a simple click button.
If the action is Nothing, the icon button is disabled.
-}
button : Maybe msg -> Suggestion msg -> Suggestion msg
button action (Suggestion pipeline) =
    Suggestion { pipeline | action = Action.Button action }


{-| Give the chip an action as a link.
Links can't be disabled.
Note that web components don't have links that feed into Elm, so links will
force a page load. If you don't want this, see [`replacedLink`](#replacedLink)
for a workaround.
Takes the link url, and the target.
-}
link : String -> Maybe String -> Suggestion msg -> Suggestion msg
link href target (Suggestion pipeline) =
    Suggestion { pipeline | action = Action.Link href target Nothing }


{-| Give the chip an action as a link.
Links can't be disabled.
This replaces the normal click action with an Elm one, half button, half link.
Takes the link url, and the target.
-}
replacedLink : (String -> Maybe String -> msg) -> String -> Maybe String -> Suggestion msg -> Suggestion msg
replacedLink pushUrl href target (Suggestion pipeline) =
    Suggestion { pipeline | action = Action.Link href target (Just pushUrl) }


{-| Indicates the chip has an icon.
-}
icon : List (Html msg) -> Suggestion msg -> Suggestion msg
icon iconElement (Suggestion pipeline) =
    Suggestion { pipeline | icon = Just iconElement }


{-| Display the chip as elevated and more important.
-}
elevated : Suggestion msg -> Suggestion msg
elevated (Suggestion pipeline) =
    Suggestion { pipeline | elevated = True }


{-| Add custom attributes to the chip.
-}
attrs : List (Html.Attribute msg) -> Suggestion msg -> Suggestion msg
attrs newAttrs (Suggestion pipeline) =
    Suggestion { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Turn a [`Suggestion` definition](#Suggestion) into HTML.
-}
view : Suggestion msg -> Html msg
view (Suggestion pipeline) =
    let
        allAttrs =
            [ [ pipeline.label |> HtmlA.attribute "label" ]
            , Action.pipelineAttrs pipeline
            , pipeline.elevated |> Util.boolAttr "elevated"
            , pipeline.attrs
            ]

        allContents =
            pipeline.icon |> Util.asSlot "icon"
    in
    Html.node "md-suggestion-chip" (List.concat allAttrs) allContents
