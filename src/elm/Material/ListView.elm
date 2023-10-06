module Material.ListView exposing
    ( ListView, Child, list, view
    , attrs
    , Item, item, itemToChild
    , disabled, link, replacedLink, button
    , start, overline, supportingText, trailingSupportingText, end
    , itemAttrs
    , Divider, divider, dividerToChild
    , dividerAttrs
    )

{-| Lists are continuous, vertical indexes of text and images, called ListView
here to avoid clashing with the data structure.


# Core

@docs ListView, Child, list, view


# Optional Customisation

@docs attrs


# Items

@docs Item, item, itemToChild


## Actions

@docs disabled, link, replacedLink, button


## Optional Customisation

@docs start, overline, supportingText, trailingSupportingText, end


### Other

@docs itemAttrs


# Dividers

@docs Divider, divider, dividerToChild


## Optional Customisation

@docs dividerAttrs

-}

import Html exposing (Html)
import Material.Util.Action as Action exposing (Action)
import Material.Util.Item as Item


type alias ItemPipeline msg =
    { action : Action msg
    , headline : List (Html msg)
    , disabled : Bool
    , overline : Maybe (List (Html msg))
    , supportingText : Maybe (List (Html msg))
    , trailingSupportingText : Maybe (List (Html msg))
    , start : Maybe (List (Html msg))
    , end : Maybe (List (Html msg))
    , attrs : List (Html.Attribute msg)
    }


type Item msg
    = Item (ItemPipeline msg)


{-| Create an item with the given label.
-}
item : List (Html msg) -> Item msg
item headline =
    ItemPipeline
        (Action.Button Nothing)
        headline
        False
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        []
        |> Item


{-| A list item that is informational, and can't be interacted with.
-}
disabled : Item msg -> Item msg
disabled (Item pipeline) =
    pipeline |> Action.disabled |> Item


{-| Give the list item an action as a simple click button.
If the action is Nothing, the icon button is disabled.
-}
button : Maybe msg -> Item msg -> Item msg
button action (Item pipeline) =
    pipeline |> Action.button action |> Item


{-| Give the list item an action as a link.
Links can't be disabled.
Unfortunately Elm link handling in web components doesn't work, so we have to
take a pushUrl message wrapper to handle that case.
Takes the link url, and the target.
-}
link : String -> Maybe String -> Item msg -> Item msg
link href target (Item pipeline) =
    pipeline |> Action.link href target |> Item


{-| Give the list item an action as a link.
Links can't be disabled.
This replaces the normal click action with an Elm one, half button, half link.
Takes the link url, and the target.
-}
replacedLink : (String -> Maybe String -> msg) -> String -> Maybe String -> Item msg -> Item msg
replacedLink pushUrl href target (Item pipeline) =
    pipeline |> Action.replacedLink pushUrl href target |> Item


{-| Add supporting text to the item.
-}
supportingText : List (Html msg) -> Item msg -> Item msg
supportingText supportingTextElement (Item pipeline) =
    pipeline |> Item.supportingText supportingTextElement |> Item


{-| Add text that displays above the headline for the item.
-}
overline : List (Html msg) -> Item msg -> Item msg
overline overlineElement (Item pipeline) =
    pipeline |> Item.overline overlineElement |> Item


{-| Add trailing supporting text to the item. Replaces any other trailing element.
-}
trailingSupportingText : List (Html msg) -> Item msg -> Item msg
trailingSupportingText trailingSupportingTextElement (Item pipeline) =
    pipeline |> Item.trailingSupportingText trailingSupportingTextElement |> Item


{-| Add an element that displays at the start of the item.
-}
start : List (Html msg) -> Item msg -> Item msg
start startElement (Item pipeline) =
    pipeline |> Item.start startElement |> Item


{-| Add an element that displays at the end of the item.
-}
end : List (Html msg) -> Item msg -> Item msg
end endElement (Item pipeline) =
    pipeline |> Item.end endElement |> Item


{-| Add custom attributes to the item.
-}
itemAttrs : List (Html.Attribute msg) -> Item msg -> Item msg
itemAttrs newAttrs (Item pipeline) =
    pipeline |> Item.attrs newAttrs |> Item


{-| Finalise the item into a child for use in a list.
-}
itemToChild : Item msg -> Child msg
itemToChild (Item pipeline) =
    ItemChild pipeline


type alias DividerPipeline msg =
    { attrs : List (Html.Attribute msg) }


type Divider msg
    = Divider (DividerPipeline msg)


{-| Create a divider.
-}
divider : Divider msg
divider =
    DividerPipeline [] |> Divider


{-| Add custom attributes to the divider.
-}
dividerAttrs : List (Html.Attribute msg) -> Divider msg -> Divider msg
dividerAttrs newAttrs (Divider pipeline) =
    Divider { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Finalise the divider into a child for use in a list.
-}
dividerToChild : Divider msg -> Child msg
dividerToChild (Divider pipeline) =
    DividerChild pipeline


{-| A child is either an item or a divider.
-}
type Child msg
    = ItemChild (ItemPipeline msg)
    | DividerChild (DividerPipeline msg)


internalViewChild : Child msg -> Html msg
internalViewChild child =
    case child of
        ItemChild pipeline ->
            Item.view "md-list-item"
                (Action.pipelineAttrs pipeline)
                []
                pipeline

        DividerChild pipeline ->
            Html.node "md-divider" pipeline.attrs []


type alias Pipeline msg =
    { children : List (Child msg)
    , attrs : List (Html.Attribute msg)
    }


{-| An list definition, which can be turned into HTML with
[`view`](#view).
-}
type ListView msg
    = ListView (Pipeline msg)


{-| Create a list from a number of children.
-}
list : List (Child msg) -> ListView msg
list children =
    Pipeline children [] |> ListView


{-| Add custom attributes to the list.
-}
attrs : List (Html.Attribute msg) -> ListView msg -> ListView msg
attrs newAttrs (ListView pipeline) =
    ListView { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Turn an [`ListView` definition](#ListView) into HTML.
-}
view : ListView msg -> Html msg
view (ListView pipeline) =
    pipeline.children
        |> List.map internalViewChild
        |> Html.node "md-list" pipeline.attrs
