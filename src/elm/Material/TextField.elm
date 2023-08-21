module Material.TextField exposing
    ( Type(..)
    , onKeyDown
    , view
    , viewWithAttrs
    , viewWithFocus
    , viewWithReturn
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Material.Attributes as HtmlA


{-| The type of field.
-}
type Type
    = Text
    | Search
    | Tel
    | Url
    | Email
    | Password
    | Date
    | Month
    | Week
    | Time
    | DateTimeLocal
    | Number
    | Color


view : String -> Type -> String -> Maybe (String -> msg) -> Html msg
view label t value action =
    viewWithAttrs label t value action []


{-| A text field for the given type of data.
-}
viewWithFocus : String -> Type -> String -> Maybe (String -> msg) -> msg -> msg -> Html msg
viewWithFocus label t value action onFocus onBlur =
    viewWithAttrs label t value action [ onFocus |> HtmlE.onFocus, onBlur |> HtmlE.onBlur ]


viewWithReturn : String -> Type -> String -> Maybe (String -> msg) -> msg -> msg -> Html msg
viewWithReturn label t value action onReturn noOp =
    let
        onKeyPress pressedKey =
            case pressedKey of
                "Enter" ->
                    onReturn

                _ ->
                    noOp
    in
    viewWithAttrs label t value action [ onKeyDown onKeyPress ]


viewWithAttrs : String -> Type -> String -> Maybe (String -> msg) -> List (Html.Attribute msg) -> Html msg
viewWithAttrs label t value action attrs =
    let
        actionAttr =
            case action of
                Just wrap ->
                    HtmlE.onInput wrap

                Nothing ->
                    HtmlA.disabled True

        allAttrs =
            [ label |> HtmlA.label
            , t |> type_
            , value |> HtmlA.value
            , actionAttr
            ]
    in
    Html.node "mwc-textfield" (allAttrs ++ attrs) []


onKeyDown : (String -> msg) -> Html.Attribute msg
onKeyDown onKeyPress =
    JsonD.field "key" JsonD.string |> JsonD.map onKeyPress |> HtmlE.on "keydown"



{- Private -}


type_ : Type -> Html.Attribute msg
type_ t =
    let
        stringType =
            case t of
                Text ->
                    "text"

                Search ->
                    "search"

                Tel ->
                    "tel"

                Url ->
                    "url"

                Email ->
                    "email"

                Password ->
                    "password"

                Date ->
                    "date"

                Month ->
                    "month"

                Week ->
                    "week"

                Time ->
                    "time"

                DateTimeLocal ->
                    "datetime-local"

                Number ->
                    "number"

                Color ->
                    "color"
    in
    stringType |> HtmlA.attribute "type"
