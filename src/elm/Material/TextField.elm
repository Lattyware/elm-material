module Material.TextField exposing
    ( TextField, view
    , filled, outlined
    , text, email, number, password, search, tel, url, textArea
    , date, time, color
    , enterAction, enterAndEscapeAction, keyPressAction
    , placeholder, supportingText
    , leadingIcon, trailingIcon, prefixText, suffixText
    , required, maxLength, error
    , attrs
    )

{-| Text fields let users enter text into a UI.


# Core

@docs TextField, view


# Variants

@docs filled, outlined


# Input Types

@docs text, email, number, password, search, tel, url, textArea
@docs date, time, color


# Optional Customisation

@docs enterAction, enterAndEscapeAction, keyPressAction
@docs placeholder, supportingText
@docs leadingIcon, trailingIcon, prefixText, suffixText
@docs required, maxLength, error
@docs attrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Material.Util as Util


type alias Pipeline msg =
    { node : String
    , label : String
    , action : Maybe (String -> msg)
    , value : String
    , type_ : String
    , placeholder : Maybe String
    , leadingIcon : Maybe (Html msg)
    , trailingIcon : Maybe (Html msg)
    , prefixText : Maybe String
    , suffixText : Maybe String
    , supportingText : Maybe String
    , maxLength : Maybe Int
    , error : Maybe String
    , attrs : List (Html.Attribute msg)
    }


{-| A text field definition, which can be turned into HTML with
[`view`](#view).
-}
type TextField msg
    = TextField (Pipeline msg)


init : String -> String -> Maybe (String -> msg) -> String -> TextField msg
init node label action value =
    Pipeline node
        label
        action
        value
        "text"
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        []
        |> TextField


{-| An outlined text field.
Takes the label and an action on input (or nothing if disabled).
-}
filled : String -> Maybe (String -> msg) -> String -> TextField msg
filled =
    init "md-filled-text-field"


{-| An outlined text field.
Takes the label and an action on input (or nothing if disabled).
-}
outlined : String -> Maybe (String -> msg) -> String -> TextField msg
outlined =
    init "md-outlined-text-field"


{-| Set the text field to expect text input, the default.
-}
text : TextField msg -> TextField msg
text (TextField pipeline) =
    TextField { pipeline | type_ = "text" }


{-| Set the text field to expect email address input.
-}
email : TextField msg -> TextField msg
email (TextField pipeline) =
    TextField { pipeline | type_ = "email" }


{-| Set the text field to expect numeric input.
-}
number : TextField msg -> TextField msg
number (TextField pipeline) =
    TextField { pipeline | type_ = "number" }


{-| Set the text field to expect password input.
-}
password : TextField msg -> TextField msg
password (TextField pipeline) =
    TextField { pipeline | type_ = "password" }


{-| Set the text field to expect search query input.
-}
search : TextField msg -> TextField msg
search (TextField pipeline) =
    TextField { pipeline | type_ = "search" }


{-| Set the text field to expect telephone number input.
-}
tel : TextField msg -> TextField msg
tel (TextField pipeline) =
    TextField { pipeline | type_ = "tel" }


{-| Set the text field to expect url input.
-}
url : TextField msg -> TextField msg
url (TextField pipeline) =
    TextField { pipeline | type_ = "url" }


{-| Set the text field to expect text area (multiline) input.
-}
textArea : TextField msg -> TextField msg
textArea (TextField pipeline) =
    TextField { pipeline | type_ = "textarea" }


{-| Set the text field to expect date input (not fully supported).
-}
date : TextField msg -> TextField msg
date (TextField pipeline) =
    TextField { pipeline | type_ = "date" }


{-| Set the text field to expect time input (not fully supported).
-}
time : TextField msg -> TextField msg
time (TextField pipeline) =
    TextField { pipeline | type_ = "time" }


{-| Set the text field to expect color input (not fully supported).
-}
color : TextField msg -> TextField msg
color (TextField pipeline) =
    TextField { pipeline | type_ = "color" }


{-| Set placeholder text for the text field, to show the user an example value.
-}
placeholder : String -> TextField msg -> TextField msg
placeholder givenPlaceholder (TextField pipeline) =
    TextField { pipeline | placeholder = Just givenPlaceholder }


{-| Set supporting text for the text field, displayed to help to user
understand what the field is for.
-}
supportingText : String -> TextField msg -> TextField msg
supportingText givenText (TextField pipeline) =
    TextField { pipeline | supportingText = Just givenText }


{-| An icon shown before the field to give additional context for the value.
-}
leadingIcon : Html msg -> TextField msg -> TextField msg
leadingIcon givenIcon (TextField pipeline) =
    TextField { pipeline | leadingIcon = Just givenIcon }


{-| An icon shown after the field to give additional context for the value.
-}
trailingIcon : Html msg -> TextField msg -> TextField msg
trailingIcon givenIcon (TextField pipeline) =
    TextField { pipeline | trailingIcon = Just givenIcon }


{-| Text before the field to give additional context for the value.
-}
prefixText : String -> TextField msg -> TextField msg
prefixText givenText (TextField pipeline) =
    TextField { pipeline | prefixText = Just givenText }


{-| Text after the field to give additional context for the value.
-}
suffixText : String -> TextField msg -> TextField msg
suffixText givenText (TextField pipeline) =
    TextField { pipeline | suffixText = Just givenText }


{-| The maximum length for the field, shows a counter.
-}
maxLength : Int -> TextField msg -> TextField msg
maxLength limit (TextField pipeline) =
    TextField { pipeline | maxLength = Just limit }


{-| Display an error, displaces the supporting text if set.
-}
error : Maybe String -> TextField msg -> TextField msg
error givenError (TextField pipeline) =
    TextField { pipeline | error = givenError }


{-| Indicates the field is required and giving no value isn't correct.
-}
required : Bool -> TextField msg -> TextField msg
required isRequired (TextField pipeline) =
    TextField { pipeline | attrs = HtmlA.required isRequired :: pipeline.attrs }


{-| An action to do whenever a key is pressed in the field.
Takes a function from the key name to a message.
-}
keyPressAction : (String -> JsonD.Decoder msg) -> TextField msg -> TextField msg
keyPressAction onKeyPress (TextField pipeline) =
    let
        onKeyDown wrap =
            JsonD.field "key" JsonD.string
                |> JsonD.andThen wrap
                |> HtmlE.on "keydown"
    in
    TextField { pipeline | attrs = onKeyDown onKeyPress :: pipeline.attrs }


{-| An action to do whenever the enter key is pressed.
-}
enterAction : msg -> TextField msg -> TextField msg
enterAction onEnter =
    let
        onKeyPress pressedKey =
            case pressedKey of
                "Enter" ->
                    JsonD.succeed onEnter

                _ ->
                    JsonD.fail "Not a monitored key."
    in
    keyPressAction onKeyPress


{-| An action to do whenever the enter key is pressed, and another when the
escape key is pressed.
-}
enterAndEscapeAction : msg -> msg -> TextField msg -> TextField msg
enterAndEscapeAction onEnter onEscape =
    let
        onKeyPress pressedKey =
            case pressedKey of
                "Enter" ->
                    JsonD.succeed onEnter

                "Escape" ->
                    JsonD.succeed onEscape

                _ ->
                    JsonD.fail "Not a monitored key."
    in
    keyPressAction onKeyPress


{-| Turn a [`TextField` definition](#TextField) into HTML.
-}
view : TextField msg -> Html msg
view (TextField pipeline) =
    let
        errorAttrs =
            case pipeline.error of
                Just givenError ->
                    [ HtmlA.attribute "error" ""
                    , HtmlA.attribute "error-text" givenError
                    ]

                Nothing ->
                    []

        allAttrs =
            [ [ HtmlA.type_ pipeline.type_
              , HtmlA.attribute "label" pipeline.label
              , Util.argActionAttr HtmlE.onInput pipeline.action
              , HtmlA.value pipeline.value
              ]
            , pipeline.placeholder |> Util.asAttr "placeholder"
            , pipeline.prefixText |> Util.asAttr "prefix-text"
            , pipeline.suffixText |> Util.asAttr "suffix-text"
            , pipeline.supportingText |> Util.asAttr "supporting-text"
            , pipeline.maxLength
                |> Maybe.map String.fromInt
                |> Util.asAttr "maxlength"
            , errorAttrs
            , pipeline.attrs
            ]

        allContents =
            [ pipeline.leadingIcon |> Util.asSlot "leadingicon"
            , pipeline.trailingIcon |> Util.asSlot "trailingicon"
            ]
    in
    Html.node pipeline.node (List.concat allAttrs) (List.concat allContents)


{-| Add custom attributes to the text field.
-}
attrs : List (Html.Attribute msg) -> TextField msg -> TextField msg
attrs newAttrs (TextField pipeline) =
    TextField { pipeline | attrs = List.append pipeline.attrs newAttrs }
