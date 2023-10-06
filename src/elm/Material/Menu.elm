module Material.Menu exposing
    ( Menu, menu, Child, view
    , onClosed, onClosing
    , AxisEnd(..), anchorCorner, menuCorner
    , FocusState(..), defaultFocus
    , fixed
    , attrs
    , Item, item, itemToChild
    , disabled, link, replacedLink, button
    , start, overline, supportingText, trailingSupportingText, end
    , itemAttrs
    , Divider, divider, dividerToChild
    , dividerAttrs
    )

{-| Menus display a list of choices on a temporary surface.


# Core

@docs Menu, menu, Child, view


# Optional Customisation

@docs onClosed, onClosing
@docs AxisEnd, anchorCorner, menuCorner
@docs FocusState, defaultFocus
@docs fixed
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
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Material.Util as Util
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


{-| Create a menu item with the given label.
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


{-| A menu item that is informational, and can't be interacted with.
-}
disabled : Item msg -> Item msg
disabled (Item pipeline) =
    pipeline |> Action.disabled |> Item


{-| Give the menu item an action as a simple click button.
If the action is Nothing, the icon button is disabled.
-}
button : Maybe msg -> Item msg -> Item msg
button action (Item pipeline) =
    pipeline |> Action.button action |> Item


{-| Give the menu item an action as a link.
Links can't be disabled.
Unfortunately Elm link handling in web components doesn't work, so we have to
take a pushUrl message wrapper to handle that case.
Takes the link url
-}
link : String -> Maybe String -> Item msg -> Item msg
link href target (Item pipeline) =
    pipeline |> Action.link href target |> Item


{-| Give the menu item an action as a link.
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
            Item.view "md-menu-item"
                (Action.pipelineAttrs pipeline)
                []
                pipeline

        DividerChild pipeline ->
            Html.node "md-divider" pipeline.attrs []


type AxisEnd
    = Start
    | End


axisEndToString : AxisEnd -> String
axisEndToString axisEnd =
    case axisEnd of
        Start ->
            "start"

        End ->
            "end"


type alias Corner =
    { block : AxisEnd, inline : AxisEnd }


cornerToString : Corner -> String
cornerToString { block, inline } =
    axisEndToString block ++ "-" ++ axisEndToString inline


type FocusState
    = None
    | ListRoot
    | FirstItem
    | LastItem


focusStateToString : FocusState -> String
focusStateToString focusState =
    case focusState of
        None ->
            "none"

        ListRoot ->
            "list-root"

        FirstItem ->
            "first-item"

        LastItem ->
            "last-item"


type alias Pipeline msg =
    { anchor : String
    , children : List (Child msg)
    , anchorCorner : Corner
    , menuCorner : Corner
    , defaultFocus : FocusState
    , open : Bool
    , fixed : Bool
    , quick : Bool
    , hasOverflow : Bool
    , attrs : List (Html.Attribute msg)
    }


{-| A menu definition, which can be turned into HTML with [`view`](#view).
-}
type Menu msg
    = Menu (Pipeline msg)


{-| A menu.
-}
menu : String -> Bool -> List (Child msg) -> Menu msg
menu anchor open children =
    Pipeline
        anchor
        children
        (Corner End Start)
        (Corner Start Start)
        ListRoot
        open
        False
        False
        False
        []
        |> Menu


{-| Which corner of the anchor the menu expands from.
-}
anchorCorner : AxisEnd -> AxisEnd -> Menu msg -> Menu msg
anchorCorner block inline (Menu pipeline) =
    Menu { pipeline | anchorCorner = Corner block inline }


{-| Which corner of the menu the menu expands from.
-}
menuCorner : AxisEnd -> AxisEnd -> Menu msg -> Menu msg
menuCorner block inline (Menu pipeline) =
    Menu { pipeline | menuCorner = Corner block inline }


{-| The element that should be focused by default once opened.
-}
defaultFocus : FocusState -> Menu msg -> Menu msg
defaultFocus focusState (Menu pipeline) =
    Menu { pipeline | defaultFocus = focusState }


{-| Make the menu fixed in position.
-}
fixed : Menu msg -> Menu msg
fixed (Menu pipeline) =
    Menu { pipeline | fixed = True }


{-| An action to do whenever the menu is closing (the start of the animation.)
-}
onClosing : msg -> Menu msg -> Menu msg
onClosing closed (Menu pipeline) =
    let
        onClosingAttr =
            closed |> JsonD.succeed |> HtmlE.on "closing"
    in
    Menu { pipeline | attrs = onClosingAttr :: pipeline.attrs }


{-| An action to do whenever the menu is closed (the end of the animation.)
-}
onClosed : msg -> Menu msg -> Menu msg
onClosed closed (Menu pipeline) =
    let
        onCloseAttr =
            closed |> JsonD.succeed |> HtmlE.on "closed"
    in
    Menu { pipeline | attrs = onCloseAttr :: pipeline.attrs }


{-| Add custom attributes to the menu.
-}
attrs : List (Html.Attribute msg) -> Menu msg -> Menu msg
attrs newAttrs (Menu pipeline) =
    Menu { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Turn a [`Menu` definition](#Menu) into HTML.
-}
view : Menu msg -> Html msg
view (Menu pipeline) =
    let
        positioning =
            if pipeline.fixed then
                "fixed"

            else
                "absolute"

        allAttrs =
            [ [ pipeline.anchorCorner |> cornerToString |> HtmlA.attribute "anchor-corner"
              , pipeline.menuCorner |> cornerToString |> HtmlA.attribute "menu-corner"
              , pipeline.defaultFocus |> focusStateToString |> HtmlA.attribute "default-focus"
              , pipeline.anchor |> HtmlA.attribute "anchor"
              , positioning |> HtmlA.attribute "positioning"
              ]
            , pipeline.open |> Util.boolAttr "open"
            , pipeline.quick |> Util.boolAttr "quick"
            , pipeline.hasOverflow |> Util.boolAttr "has-overflow"
            , pipeline.attrs
            ]
    in
    pipeline.children
        |> List.map internalViewChild
        |> Html.node "md-menu" (List.concat allAttrs)
