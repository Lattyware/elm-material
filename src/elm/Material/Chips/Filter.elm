module Material.Chips.Filter exposing
    ( Filter, chip, view
    , button, link, replacedLink
    , elevated, selected, attrs
    )

{-| Filter chips use tags or descriptive words to filter content.
They can be a good alternative to toggle buttons or checkboxes.


# Core

@docs Filter, chip, view


# Actions

@docs button, link, replacedLink


# Optional Customisation

@docs elevated, selected, attrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Material.Util as Util
import Material.Util.Action as Action exposing (Action)


type alias Pipeline msg =
    { label : String
    , icon : Maybe (List (Html msg))
    , elevated : Bool
    , selected : Bool
    , action : Action msg
    , attrs : List (Html.Attribute msg)
    }


{-| A filter chip.
-}
type Filter msg
    = Filter (Pipeline msg)


{-| A filter chip, taking its label.
-}
chip : String -> Filter msg
chip label =
    Pipeline
        label
        Nothing
        False
        False
        (Action.Button Nothing)
        []
        |> Filter


{-| Give the chip an action as a simple click button.
If the action is Nothing, the icon button is disabled.
-}
button : Maybe msg -> Filter msg -> Filter msg
button action (Filter pipeline) =
    Filter { pipeline | action = Action.Button action }


{-| Give the chip an action as a link.
Links can't be disabled.
Note that web components don't have links that feed into Elm, so links will
force a page load. If you don't want this, see [`replacedLink`](#replacedLink)
for a workaround.
Takes the link url, and the target.
-}
link : String -> Maybe String -> Filter msg -> Filter msg
link href target (Filter pipeline) =
    Filter { pipeline | action = Action.Link href target Nothing }


{-| Give the chip an action as a link.
Links can't be disabled.
This replaces the normal click action with an Elm one, half button, half link.
Takes the link url, and the target.
-}
replacedLink : (String -> Maybe String -> msg) -> String -> Maybe String -> Filter msg -> Filter msg
replacedLink pushUrl href target (Filter pipeline) =
    Filter { pipeline | action = Action.Link href target (Just pushUrl) }


{-| Display the chip as elevated and more important.
-}
elevated : Filter msg -> Filter msg
elevated (Filter pipeline) =
    Filter { pipeline | elevated = True }


{-| Display the chip as selected.
-}
selected : Bool -> Filter msg -> Filter msg
selected isSelected (Filter pipeline) =
    Filter { pipeline | selected = isSelected }


{-| Add custom attributes to the chip.
-}
attrs : List (Html.Attribute msg) -> Filter msg -> Filter msg
attrs newAttrs (Filter pipeline) =
    Filter { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Turn a [`Filter` definition](#Filter) into HTML.
-}
view : Filter msg -> Html msg
view (Filter pipeline) =
    let
        allAttrs =
            [ [ pipeline.label |> HtmlA.attribute "label" ]
            , Action.pipelineAttrs pipeline
            , pipeline.elevated |> Util.boolAttr "elevated"
            , pipeline.selected |> Util.boolAttr "selected"
            , pipeline.attrs
            ]

        allContents =
            pipeline.icon |> Util.asSlot "icon"
    in
    Html.node "md-Filter-chip" (List.concat allAttrs) allContents
