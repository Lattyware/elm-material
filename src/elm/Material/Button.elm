module Material.Button exposing
    ( Button, view
    , elevated, filled, filledTonal, outlined, text
    , button, link, replacedLink
    , icon, attrs
    )

{-| Buttons help people initiate actions, from sending an email, to sharing a
document, to liking a post.


# Core

@docs Button, view


# Button Variants

@docs elevated, filled, filledTonal, outlined, text


# Actions

@docs button, link, replacedLink


# Optional Customisation

@docs icon, attrs

-}

import Html exposing (Html)
import Material.Util as Util
import Material.Util.Action as Action exposing (Action)


type alias Pipeline msg =
    { node : String
    , action : Action msg
    , label : String
    , icon : Maybe (List (Html msg))
    , attrs : List (Html.Attribute msg)
    }


{-| A button definition, which can be turned into HTML with [`view`](#view).
-}
type Button msg
    = Button (Pipeline msg)


init : String -> String -> Button msg
init node label =
    Pipeline node (Action.Button Nothing) label Nothing [] |> Button


{-| Elevated buttons are essentially filled tonal buttons with a shadow.
To prevent shadow creep, only use them when absolutely necessary, such as when
the button requires visual separation from a patterned background.
Takes a label
-}
elevated : String -> Button msg
elevated =
    init "md-elevated-button"


{-| Filled buttons have the most visual impact after the FAB, and should be
used for important, final actions that complete a flow, like Save, Join now,
or Confirm.
Takes a label.
-}
filled : String -> Button msg
filled =
    init "md-filled-button"


{-| A filled tonal button is an alternative middle ground between filled and
outlined buttons. They're useful in contexts where a lower-priority button
requires slightly more emphasis than an outline would give, such as "Next" in
an onboarding flow.
Takes a label.
-}
filledTonal : String -> Button msg
filledTonal =
    init "md-filled-tonal-button"


{-| Outlined buttons are medium-emphasis buttons. They contain actions that
are important, but aren’t the primary action in an app.
Takes a label.
-}
outlined : String -> Button msg
outlined =
    init "md-outlined-button"


{-| Text buttons are used for the lowest priority actions, especially when
presenting multiple options.
Takes a label.
-}
text : String -> Button msg
text =
    init "md-text-button"


{-| Give the button an action as a simple click button.
If the action is Nothing, the icon button is disabled.
-}
button : Maybe msg -> Button msg -> Button msg
button action (Button pipeline) =
    Button { pipeline | action = Action.Button action }


{-| Give the button an action as a link.
Links can't be disabled.
Note that web components don't have links that feed into Elm, so links will
force a page load. If you don't want this, see [`replacedLink`](#replacedLink)
for a workaround.
Takes the link url, and the target.
-}
link : String -> Maybe String -> Button msg -> Button msg
link href target (Button pipeline) =
    Button { pipeline | action = Action.Link href target Nothing }


{-| Give the button an action as a link.
Links can't be disabled.
This replaces the normal click action with an Elm one, half button, half link.
Takes the link url, and the target.
-}
replacedLink : (String -> Maybe String -> msg) -> String -> Maybe String -> Button msg -> Button msg
replacedLink pushUrl href target (Button pipeline) =
    Button { pipeline | action = Action.Link href target (Just pushUrl) }


{-| Set an icon for a button to help communicate the button's action and help
draw attention.
-}
icon : List (Html msg) -> Button msg -> Button msg
icon newIcon (Button pipeline) =
    Button { pipeline | icon = Just newIcon }


{-| Add custom attributes to the button.
-}
attrs : List (Html.Attribute msg) -> Button msg -> Button msg
attrs newAttrs (Button pipeline) =
    Button { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Turn a [`Button` definition](#Button) into HTML.
-}
view : Button msg -> Html msg
view (Button pipeline) =
    Html.node pipeline.node
        (List.append (Action.pipelineAttrs pipeline) pipeline.attrs)
        (Html.text pipeline.label :: (pipeline.icon |> Util.asSlot "icon"))
