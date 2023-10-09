module Material.Chips.Assist exposing
    ( Assist, chip, view
    , button, link, replacedLink
    , icon, elevated, attrs
    )

{-| Assist chips represent smart or automated actions that can span multiple
apps, such as opening a calendar event from the home screen.


# Core

@docs Assist, chip, view


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


{-| An assist chip.
-}
type Assist msg
    = Assist (Pipeline msg)


{-| An assist chip, taking its label.
-}
chip : String -> Assist msg
chip label =
    Pipeline
        label
        Nothing
        False
        (Action.Button Nothing)
        []
        |> Assist


{-| Give the chip an action as a simple click button.
If the action is Nothing, the icon button is disabled.
-}
button : Maybe msg -> Assist msg -> Assist msg
button action (Assist pipeline) =
    Assist { pipeline | action = Action.Button action }


{-| Give the chip an action as a link.
Links can't be disabled.
Note that web components don't have links that feed into Elm, so links will
force a page load. If you don't want this, see [`replacedLink`](#replacedLink)
for a workaround.
Takes the link url, and the target.
-}
link : String -> Maybe String -> Assist msg -> Assist msg
link href target (Assist pipeline) =
    Assist { pipeline | action = Action.Link href target Nothing }


{-| Give the chip an action as a link.
Links can't be disabled.
This replaces the normal click action with an Elm one, half button, half link.
Takes the link url, and the target.
-}
replacedLink : (String -> Maybe String -> msg) -> String -> Maybe String -> Assist msg -> Assist msg
replacedLink pushUrl href target (Assist pipeline) =
    Assist { pipeline | action = Action.Link href target (Just pushUrl) }


{-| Indicates the chip has an icon.
-}
icon : List (Html msg) -> Assist msg -> Assist msg
icon iconElement (Assist pipeline) =
    Assist { pipeline | icon = Just iconElement }


{-| Display the chip as elevated and more important.
-}
elevated : Assist msg -> Assist msg
elevated (Assist pipeline) =
    Assist { pipeline | elevated = True }


{-| Add custom attributes to the chip.
-}
attrs : List (Html.Attribute msg) -> Assist msg -> Assist msg
attrs newAttrs (Assist pipeline) =
    Assist { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Turn a [`Assist` definition](#Assist) into HTML.
-}
view : Assist msg -> Html msg
view (Assist pipeline) =
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
    Html.node "md-assist-chip" (List.concat allAttrs) allContents
