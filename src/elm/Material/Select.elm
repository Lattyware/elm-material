module Material.Select exposing
    ( Select, filled, outlined, view
    , error, required, supportingText, leadingIcon, fixed
    , attrs
    , Option, option
    , icon, avatar, image, video
    , optionSupportingText
    , trailingSupportingText, trailingIcon
    , optionAttrs
    )

{-| Selects allow users to pick a value from a range of options.


# Core

@docs Select, filled, outlined, view


# Optional Customisation

@docs error, required, supportingText, leadingIcon, fixed
@docs attrs


# Options

@docs Option, option


## Optional Customisation


### Start Parts

@docs icon, avatar, image, video


### Main parts.

@docs optionSupportingText


### End Parts

@docs trailingSupportingText, trailingIcon


### Other

@docs optionAttrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Json.Decode as JsonD
import Material.Util as Util


type Start msg
    = NoStart
    | StartSlot String (Html msg)


type End msg
    = NoEnd
    | EndIcon (Html msg)
    | EndText String


type alias OptionPipeline msg =
    { headline : String
    , selected : Bool
    , value : String
    , supportingText : Maybe String
    , multilineSupportingText : Bool
    , start : Start msg
    , end : End msg
    , error : Maybe String
    , required : Bool
    , attrs : List (Html.Attribute msg)
    }


type Option msg
    = Option (OptionPipeline msg)


{-| Create an option with the given label and value.
-}
option : String -> Bool -> String -> Option msg
option headline selected value =
    OptionPipeline headline
        selected
        value
        Nothing
        False
        NoStart
        NoEnd
        Nothing
        False
        []
        |> Option


{-| Add supporting text to the item.
Takes the text and if the text can go multiline or will be cut off.
-}
optionSupportingText : String -> Bool -> Option msg -> Option msg
optionSupportingText text multiline (Option pipeline) =
    Option
        { pipeline
            | supportingText = Just text
            , multilineSupportingText = multiline
        }


{-| Add an icon to the item. Replaces any other starting element.
-}
icon : Html msg -> Option msg -> Option msg
icon givenSlot (Option pipeline) =
    Option { pipeline | start = StartSlot "icon" givenSlot }


{-| Add an icon to the item. Replaces any other starting element.
-}
avatar : Html msg -> Option msg -> Option msg
avatar givenSlot (Option pipeline) =
    Option { pipeline | start = StartSlot "avatar" givenSlot }


{-| Add an image to the item. Replaces any other starting element.
-}
image : Html msg -> Option msg -> Option msg
image givenSlot (Option pipeline) =
    Option { pipeline | start = StartSlot "image" givenSlot }


{-| Add a video to the item. Replaces any other starting element.
-}
video : Html msg -> Option msg -> Option msg
video givenSlot (Option pipeline) =
    Option { pipeline | start = StartSlot "video" givenSlot }


{-| Add trailing supporting text to the item. Replaces any other trailing element.
-}
trailingSupportingText : String -> Option msg -> Option msg
trailingSupportingText text (Option pipeline) =
    Option { pipeline | end = EndText text }


{-| Add a trailing icon to the item. Replaces any other trailing element.
-}
trailingIcon : Html msg -> Option msg -> Option msg
trailingIcon givenIcon (Option pipeline) =
    Option { pipeline | end = EndIcon givenIcon }


{-| Add custom attributes to the item.
-}
optionAttrs : List (Html.Attribute msg) -> Option msg -> Option msg
optionAttrs newAttrs (Option pipeline) =
    Option { pipeline | attrs = List.append pipeline.attrs newAttrs }


internalViewOption : Option msg -> Html msg
internalViewOption (Option pipeline) =
    let
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
            [ [ HtmlA.attribute "headline" pipeline.headline
              , HtmlA.value pipeline.value
              ]
            , pipeline.selected |> Util.boolAttr "selected"
            , pipeline.supportingText |> Util.asAttr "supporting-text"
            , pipeline.multilineSupportingText |> Util.boolAttr "multi-line-supporting-text"
            , endAttr
            ]

        contents =
            [ startContents, endContents ]
    in
    Html.node "md-select-option"
        (List.concat attrsForItem)
        (List.concat contents)


type alias Pipeline msg =
    { node : String
    , label : String
    , options : List (Option msg)
    , action : Maybe (String -> msg)
    , quick : Bool
    , menuFixed : Bool
    , required : Bool
    , supportingText : Maybe String
    , multilineSupportingText : Bool
    , leadingIcon : Maybe (Html msg)
    , error : Maybe String
    , attrs : List (Html.Attribute msg)
    }


{-| A select definition, which can be turned into HTML with [`view`](#view).
-}
type Select msg
    = Select (Pipeline msg)


{-| A filled select.
-}
filled : String -> Maybe (String -> msg) -> List (Option msg) -> Select msg
filled label action children =
    Pipeline
        "md-filled-select"
        label
        children
        action
        False
        False
        False
        Nothing
        False
        Nothing
        Nothing
        []
        |> Select


{-| A filled select.
-}
outlined : String -> Maybe (String -> msg) -> List (Option msg) -> Select msg
outlined label action children =
    Pipeline
        "md-outlined-select"
        label
        children
        action
        False
        False
        False
        Nothing
        False
        Nothing
        Nothing
        []
        |> Select


{-| Make the select's drop down menu use a fixed position.
-}
fixed : Select msg -> Select msg
fixed (Select pipeline) =
    Select { pipeline | menuFixed = True }


{-| Add supporting text to the select.
Takes the text and if the text can go multiline or will be cut off.
-}
supportingText : String -> Bool -> Select msg -> Select msg
supportingText text multiline (Select pipeline) =
    Select
        { pipeline
            | supportingText = Just text
            , multilineSupportingText = multiline
        }


{-| Add a leading icon.
-}
leadingIcon : Html msg -> Select msg -> Select msg
leadingIcon givenIcon (Select pipeline) =
    Select { pipeline | leadingIcon = Just givenIcon }


{-| Display an error, displaces the supporting text if set.
-}
error : Maybe String -> Select msg -> Select msg
error givenError (Select pipeline) =
    Select { pipeline | error = givenError }


{-| Indicates the field is required and giving no value isn't correct.
-}
required : Select msg -> Select msg
required (Select pipeline) =
    Select { pipeline | required = True }


{-| Add custom attributes to the select.
-}
attrs : List (Html.Attribute msg) -> Select msg -> Select msg
attrs newAttrs (Select pipeline) =
    Select { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Turn a [`Select` definition](#Select) into HTML.
-}
view : Select msg -> Html msg
view (Select pipeline) =
    let
        errorAttrs =
            case pipeline.error of
                Just givenError ->
                    [ HtmlA.attribute "error" ""
                    , HtmlA.attribute "error-text" givenError
                    ]

                Nothing ->
                    []

        getValue =
            JsonD.at [ "target", "value" ] JsonD.string

        allAttrs =
            [ [ HtmlA.attribute "label" pipeline.label
              , Util.onActionAttr "change" getValue pipeline.action
              ]
            , pipeline.supportingText |> Util.asAttr "supporting-text"
            , pipeline.multilineSupportingText |> Util.boolAttr "multi-line-supporting-text"
            , pipeline.quick |> Util.boolAttr "quick"
            , pipeline.menuFixed |> Util.boolAttr "menu-fixed"
            , pipeline.required |> Util.boolAttr "required"
            , errorAttrs
            , pipeline.attrs
            ]
    in
    List.append
        (Util.asSlot "leadingicon" pipeline.leadingIcon)
        (pipeline.options |> List.map internalViewOption)
        |> Html.node pipeline.node (List.concat allAttrs)
