module Material.ListView exposing
    ( ListView, Child, list, view
    , attrs
    , Item, item, itemToChild
    , noninteractive, link, replacedLink, button
    , icon, avatar, image, video
    , supportingText
    , trailingSupportingText, trailingIcon
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

@docs noninteractive, link, replacedLink, button


## Optional Customisation


### Start Parts

@docs icon, avatar, image, video


### Main parts.

@docs supportingText


### End Parts

@docs trailingSupportingText, trailingIcon


### Other

@docs itemAttrs


# Dividers

@docs Divider, divider, dividerToChild


## Optional Customisation

@docs dividerAttrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Material.Util as Util


type Action msg
    = NonInteractive
    | Link String (Maybe String) (Maybe (String -> Maybe String -> msg))
    | Button (Maybe msg)


type Start msg
    = NoStart
    | StartSlot String (Html msg)


type End msg
    = NoEnd
    | EndIcon (Html msg)
    | EndText String


type alias ItemPipeline msg =
    { headline : String
    , supportingText : Maybe String
    , multilineSupportingText : Bool
    , action : Action msg
    , start : Start msg
    , end : End msg
    , attrs : List (Html.Attribute msg)
    }


type Item msg
    = Item (ItemPipeline msg)


{-| Create an item with the given label.
-}
item : String -> Item msg
item headline =
    ItemPipeline headline Nothing False NonInteractive NoStart NoEnd [] |> Item


{-| A list item that is informational, and can't be interacted with.
-}
noninteractive : Item msg -> Item msg
noninteractive (Item pipeline) =
    Item { pipeline | action = NonInteractive }


{-| Give the list item an action as a simple click button.
If the action is Nothing, the icon button is disabled.
-}
button : Maybe msg -> Item msg -> Item msg
button action (Item pipeline) =
    Item { pipeline | action = Button action }


{-| Give the list item an action as a link.
Links can't be disabled.
Unfortunately Elm link handling in web components doesn't work, so we have to
take a pushUrl message wrapper to handle that case.
Takes the link url, and the target.
-}
link : String -> Maybe String -> Item msg -> Item msg
link href target (Item pipeline) =
    Item { pipeline | action = Link href target Nothing }


{-| Give the list item an action as a link.
Links can't be disabled.
This replaces the normal click action with an Elm one, half button, half link.
Takes the link url, and the target.
-}
replacedLink : (String -> Maybe String -> msg) -> String -> Maybe String -> Item msg -> Item msg
replacedLink pushUrl href target (Item pipeline) =
    Item { pipeline | action = Link href target (Just pushUrl) }


{-| Add supporting text to the item.
Takes the text and if the text can go multiline or will be cut off.
-}
supportingText : String -> Bool -> Item msg -> Item msg
supportingText text multiline (Item pipeline) =
    Item
        { pipeline
            | supportingText = Just text
            , multilineSupportingText = multiline
        }


{-| Add an icon to the item. Replaces any other starting element.
-}
icon : Html msg -> Item msg -> Item msg
icon givenSlot (Item pipeline) =
    Item { pipeline | start = StartSlot "icon" givenSlot }


{-| Add an icon to the item. Replaces any other starting element.
-}
avatar : Html msg -> Item msg -> Item msg
avatar givenSlot (Item pipeline) =
    Item { pipeline | start = StartSlot "avatar" givenSlot }


{-| Add an image to the item. Replaces any other starting element.
-}
image : Html msg -> Item msg -> Item msg
image givenSlot (Item pipeline) =
    Item { pipeline | start = StartSlot "image" givenSlot }


{-| Add a video to the item. Replaces any other starting element.
-}
video : Html msg -> Item msg -> Item msg
video givenSlot (Item pipeline) =
    Item { pipeline | start = StartSlot "video" givenSlot }


{-| Add trailing supporting text to the item. Replaces any other trailing element.
-}
trailingSupportingText : String -> Item msg -> Item msg
trailingSupportingText text (Item pipeline) =
    Item { pipeline | end = EndText text }


{-| Add a trailing icon to the item. Replaces any other trailing element.
-}
trailingIcon : Html msg -> Item msg -> Item msg
trailingIcon givenIcon (Item pipeline) =
    Item { pipeline | end = EndIcon givenIcon }


{-| Add custom attributes to the item.
-}
itemAttrs : List (Html.Attribute msg) -> Item msg -> Item msg
itemAttrs newAttrs (Item pipeline) =
    Item { pipeline | attrs = List.append pipeline.attrs newAttrs }


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
            let
                actionAttrs =
                    case pipeline.action of
                        NonInteractive ->
                            [ HtmlA.attribute "noninteractive" "" ]

                        Link pushUrl href target ->
                            Util.replacedLink pushUrl href target

                        Button action ->
                            [ action |> Util.actionAttr HtmlE.onClick ]

                startContents =
                    case pipeline.start of
                        NoStart ->
                            []

                        StartSlot variant slot ->
                            [ Html.span
                                [ HtmlA.attribute "slot" "start"
                                , HtmlA.attribute "data-variant" variant
                                ]
                                [ slot ]
                            ]

                ( endContents, endAttr ) =
                    case pipeline.end of
                        NoEnd ->
                            ( [], [] )

                        EndText text ->
                            ( []
                            , [ HtmlA.attribute "trailing-supporting-text" text ]
                            )

                        EndIcon slot ->
                            ( [ Html.span
                                    [ HtmlA.attribute "slot" "end"
                                    , HtmlA.attribute "data-variant" "icon"
                                    ]
                                    [ slot ]
                              ]
                            , []
                            )

                attrsForItem =
                    [ [ HtmlA.attribute "headline" pipeline.headline ]
                    , pipeline.supportingText
                        |> Util.asAttr "supporting-text"
                    , pipeline.multilineSupportingText
                        |> Util.boolAttr "multi-line-supporting-text"
                    , actionAttrs
                    , endAttr
                    ]

                contents =
                    List.append startContents endContents
            in
            Html.node "md-list-item" (List.concat attrsForItem) contents

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
