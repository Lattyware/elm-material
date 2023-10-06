module Material.Chips.Input exposing
    ( Input, chip, view
    , button, link, replacedLink
    , icon, selected, attrs
    )

{-| Input chips represent discrete pieces of information entered by a user,
such as Gmail contacts or filter options within a search field.


# Core

@docs Input, chip, view


# Actions

@docs button, link, replacedLink


# Optional Customisation

@docs icon, selected, attrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Material.Util as Util
import Material.Util.Action as Action exposing (Action)


type alias Pipeline msg =
    { label : String
    , remove : Maybe msg
    , icon : Maybe ( List (Html msg), Bool )
    , selected : Bool
    , avatar : Bool
    , action : Maybe (Action msg)
    , attrs : List (Html.Attribute msg)
    }


{-| An input chip.
-}
type Input msg
    = Input (Pipeline msg)


{-| An input chip, taking its label and the action to remove it.
-}
chip : String -> Maybe msg -> Input msg
chip label remove =
    Pipeline
        label
        remove
        Nothing
        False
        False
        Nothing
        []
        |> Input


{-| Give the chip an action as a simple click button.
If the action is Nothing, the icon button is disabled.
-}
button : Maybe msg -> Input msg -> Input msg
button action (Input pipeline) =
    Input { pipeline | action = Just (Action.Button action) }


{-| Give the chip an action as a link.
Links can't be disabled.
Note that web components don't have links that feed into Elm, so links will
force a page load. If you don't want this, see [`replacedLink`](#replacedLink)
for a workaround.
Takes the link url, and the target.
-}
link : String -> Maybe String -> Input msg -> Input msg
link href target (Input pipeline) =
    Input { pipeline | action = Just (Action.Link href target Nothing) }


{-| Give the chip an action as a link.
Links can't be disabled.
This replaces the normal click action with an Elm one, half button, half link.
Takes the link url, and the target.
-}
replacedLink : (String -> Maybe String -> msg) -> String -> Maybe String -> Input msg -> Input msg
replacedLink pushUrl href target (Input pipeline) =
    Input { pipeline | action = Just (Action.Link href target (Just pushUrl)) }


{-| Display the chip as selected.
-}
selected : Bool -> Input msg -> Input msg
selected isSelected (Input pipeline) =
    Input { pipeline | selected = isSelected }


{-| Indicates the chip has an icon, and if it is an avatar.
-}
icon : List (Html msg) -> Bool -> Input msg -> Input msg
icon iconElement isAvatar (Input pipeline) =
    Input { pipeline | icon = Just ( iconElement, isAvatar ) }


{-| Add custom attributes to the chip.
-}
attrs : List (Html.Attribute msg) -> Input msg -> Input msg
attrs newAttrs (Input pipeline) =
    Input { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Turn a [`Input` definition](#Input) into HTML.
-}
view : Input msg -> Html msg
view (Input pipeline) =
    let
        onRemove msg =
            HtmlE.preventDefaultOn "remove" (JsonD.succeed ( msg, True ))

        actionAttrs =
            case pipeline.action of
                Just action ->
                    action |> Action.attrs

                Nothing ->
                    Util.boolAttr "remove-only" True

        allAttrs =
            [ [ pipeline.label |> HtmlA.attribute "label"
              , pipeline.remove |> Util.actionAttr onRemove
              ]
            , pipeline.selected |> Util.boolAttr "selected"
            , actionAttrs
            , pipeline.icon
                |> Maybe.map Tuple.second
                |> Maybe.withDefault False
                |> Util.boolAttr "avatar"
            , pipeline.attrs
            ]

        allContents =
            pipeline.icon |> Maybe.map Tuple.first |> Util.asSlot "icon"
    in
    Html.node "md-input-chip" (List.concat allAttrs) allContents
