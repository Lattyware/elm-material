module Material.Select exposing
    ( Select, filled, outlined, view
    , error, required, supportingText, leadingIcon, fixed
    , attrs
    , Option, option
    , displayText
    , start, overline, optionSupportingText, trailingSupportingText, end
    , optionAttrs
    , trailingIcon
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

@docs displayText


### Additional parts.

@docs start, overline, optionSupportingText, trailingSupportingText, end


### Other

@docs optionAttrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Json.Decode as JsonD
import Material.Util as Util
import Material.Util.Item as Item


type alias OptionPipeline msg =
    { headline : List (Html msg)
    , disabled : Bool
    , value : String
    , displayText : Maybe String
    , overline : Maybe (List (Html msg))
    , supportingText : Maybe (List (Html msg))
    , trailingSupportingText : Maybe (List (Html msg))
    , start : Maybe (List (Html msg))
    , end : Maybe (List (Html msg))
    , attrs : List (Html.Attribute msg)
    }


type Option msg
    = Option (OptionPipeline msg)


{-| Create an option with the given label and value.
-}
option : List (Html msg) -> String -> Option msg
option headline value =
    OptionPipeline
        headline
        False
        value
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        []
        |> Option


{-| The text that will be displayed when the option is selected, defaults to the text
from the headline element.
-}
displayText : String -> Option msg -> Option msg
displayText text (Option pipeline) =
    { pipeline | displayText = Just text } |> Option


{-| Add supporting text to the option.
-}
optionSupportingText : List (Html msg) -> Option msg -> Option msg
optionSupportingText supportingTextElement (Option pipeline) =
    pipeline |> Item.supportingText supportingTextElement |> Option


{-| Add text that displays above the headline for the option.
-}
overline : List (Html msg) -> Option msg -> Option msg
overline overlineElement (Option pipeline) =
    pipeline |> Item.overline overlineElement |> Option


{-| Add trailing supporting text to the iteoptionm. Replaces any other trailing element.
-}
trailingSupportingText : List (Html msg) -> Option msg -> Option msg
trailingSupportingText trailingSupportingTextElement (Option pipeline) =
    pipeline |> Item.trailingSupportingText trailingSupportingTextElement |> Option


{-| Add an element that displays at the start of the option.
-}
start : List (Html msg) -> Option msg -> Option msg
start startElement (Option pipeline) =
    pipeline |> Item.start startElement |> Option


{-| Add an element that displays at the end of the option.
-}
end : List (Html msg) -> Option msg -> Option msg
end endElement (Option pipeline) =
    pipeline |> Item.end endElement |> Option


{-| Add custom attributes to the option.
-}
optionAttrs : List (Html.Attribute msg) -> Option msg -> Option msg
optionAttrs newAttrs (Option pipeline) =
    pipeline |> Item.attrs newAttrs |> Option


internalViewOption : Maybe String -> Option msg -> Html msg
internalViewOption selected (Option pipeline) =
    Item.view "md-select-option"
        (List.concat
            [ [ HtmlA.attribute "value" pipeline.value ]
            , (Just pipeline.value == selected) |> Util.boolAttr "selected"
            , pipeline.displayText |> Util.asAttr "display-text"
            ]
        )
        []
        pipeline


type alias Pipeline msg =
    { node : String
    , label : String
    , options : List (Option msg)
    , action : Maybe (String -> msg)
    , quick : Bool
    , menuFixed : Bool
    , required : Bool
    , supportingText : Maybe String
    , leadingIcon : Maybe (List (Html msg))
    , trailingIcon : Maybe (List (Html msg))
    , error : Maybe String
    , selected : Maybe String
    , attrs : List (Html.Attribute msg)
    }


{-| A select definition, which can be turned into HTML with [`view`](#view).
-}
type Select msg
    = Select (Pipeline msg)


{-| A filled select.
-}
filled : String -> Maybe (String -> msg) -> Maybe String -> List (Option msg) -> Select msg
filled label action selected children =
    Pipeline
        "md-filled-select"
        label
        children
        action
        False
        False
        False
        Nothing
        Nothing
        Nothing
        Nothing
        selected
        []
        |> Select


{-| A filled select.
-}
outlined : String -> Maybe (String -> msg) -> Maybe String -> List (Option msg) -> Select msg
outlined label action selected children =
    Pipeline
        "md-outlined-select"
        label
        children
        action
        False
        False
        False
        Nothing
        Nothing
        Nothing
        Nothing
        selected
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
supportingText : String -> Select msg -> Select msg
supportingText text (Select pipeline) =
    Select { pipeline | supportingText = Just text }


{-| Add a leading icon.
-}
leadingIcon : List (Html msg) -> Select msg -> Select msg
leadingIcon givenIcon (Select pipeline) =
    Select { pipeline | leadingIcon = Just givenIcon }


{-| Add a trailing icon.
-}
trailingIcon : List (Html msg) -> Select msg -> Select msg
trailingIcon givenIcon (Select pipeline) =
    Select { pipeline | trailingIcon = Just givenIcon }


{-| Display an error, displaces the supporting text if set.
-}
error : Maybe String -> Select msg -> Select msg
error givenError (Select pipeline) =
    Select { pipeline | error = givenError }


{-| Indicates the field is required and giving no value isn't correct.
-}
required : Bool -> Select msg -> Select msg
required isRequired (Select pipeline) =
    Select { pipeline | required = isRequired }


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

        positioning =
            if pipeline.menuFixed then
                "fixed"

            else
                "absolute"

        getValue =
            JsonD.at [ "target", "value" ] JsonD.string

        allAttrs =
            [ [ HtmlA.attribute "label" pipeline.label
              , Util.onActionAttr "change" getValue pipeline.action
              , positioning |> HtmlA.attribute "menu-positioning"
              ]
            , pipeline.supportingText |> Util.asAttr "supporting-text"
            , pipeline.quick |> Util.boolAttr "quick"
            , pipeline.required |> Util.boolAttr "required"
            , errorAttrs
            , pipeline.attrs
            ]

        icons =
            [ pipeline.leadingIcon |> Util.asSlot "leading-icon"
            , pipeline.trailingIcon |> Util.asSlot "trailing-icon"
            ]
                |> List.concat
    in
    List.append
        icons
        (pipeline.options |> List.map (internalViewOption pipeline.selected))
        |> Html.node pipeline.node (List.concat allAttrs)
