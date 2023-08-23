module Material.Select exposing
    ( ItemModel
    , Model
    , view
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Json.Encode as JsonE
import Material.Attributes as HtmlA


{-| A combo box.
-}
view : Model id msg -> List (ItemModel id msg) -> Html msg
view model items =
    let
        fullWidth =
            if model.fullWidth then
                [ HtmlA.fullWidth ]

            else
                []

        fixedMenuPosition =
            if model.fixedPosition then
                [ HtmlA.attribute "fixedMenuPosition" "" ]

            else
                []

        ( onChangeAttr, disabled ) =
            case model.wrap of
                Just wrap ->
                    ( [ model.idFromString >> wrap |> onChange ], False )

                Nothing ->
                    ( [], True )

        allAttrs =
            List.concat
                [ model.attrs
                , [ model.label |> HtmlA.label
                  , HtmlA.disabled (model.disabled || disabled)
                  ]
                , onChangeAttr
                , fixedMenuPosition
                , fullWidth
                ]
    in
    Html.node "mwc-select" allAttrs (items |> List.map (viewItem model))


{-| The things needed to render the select.
-}
type alias Model id msg =
    { label : String
    , idToString : id -> String
    , idFromString : String -> Maybe id
    , selected : Maybe id
    , wrap : Maybe (Maybe id -> msg)
    , disabled : Bool
    , fullWidth : Bool
    , fixedPosition : Bool
    , attrs : List (Html.Attribute msg)
    }


{-| The things needed to render a specific item in the select.
-}
type alias ItemModel id msg =
    { id : id
    , icon : Maybe (Html msg)
    , primary : List (Html msg)
    , secondary : Maybe (List (Html msg))
    , meta : Maybe (Html msg)
    }



{- Private -}


{-| An item within a list.
-}
viewItem : Model id msg -> ItemModel id msg -> Html msg
viewItem { idToString, selected } { id, icon, primary, secondary, meta } =
    let
        iconAttrAndSlot i =
            ( HtmlA.attribute "graphic" "large"
            , Html.span [ HtmlA.slot "graphic" ] [ i ]
            )

        metaAttrAndSlot m =
            ( True |> JsonE.bool |> HtmlA.property "hasMeta"
            , Html.span [ HtmlA.slot "meta" ] [ m ]
            )

        secondaryAttrAndSlot s =
            ( True |> JsonE.bool |> HtmlA.property "twoline"
            , Html.span [ HtmlA.slot "secondary" ] s
            )

        ( optionalAttrs, optionalSlots ) =
            [ icon |> Maybe.map iconAttrAndSlot
            , meta |> Maybe.map metaAttrAndSlot
            , secondary |> Maybe.map secondaryAttrAndSlot
            ]
                |> List.filterMap identity
                |> List.unzip

        attrs =
            (id |> idToString |> HtmlA.value)
                :: (selected == Just id |> HtmlA.selected)
                :: optionalAttrs

        slots =
            List.concat [ primary, optionalSlots ]
                |> List.intersperse (Html.text " ")
    in
    Html.node "mwc-list-item" attrs slots


{-| An event for when the user changes the selection.
-}
onChange : (String -> msg) -> Html.Attribute msg
onChange wrap =
    JsonD.at [ "target", "value" ] JsonD.string
        |> JsonD.map wrap
        |> HtmlE.on "change"
