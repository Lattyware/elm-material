module Material.Tabs exposing
    ( Model
    , TabModel
    , view
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Json.Encode as JsonE
import List.Extra as List
import Material.Attributes as HtmlA


{-| A model for a bar of tabs.
-}
type alias Model id msg =
    { selected : id
    , change : Maybe id -> msg
    , ids : List id
    , tab : id -> TabModel msg
    , equals : id -> id -> Bool
    , stacked : Bool
    }


{-| A model for a tab.
-}
type alias TabModel msg =
    { label : String
    , icon : Maybe (Html msg)
    }


{-| View a bar of tabs.
-}
view : Model id msg -> Html msg
view { selected, change, ids, tab, equals, stacked } =
    let
        stackedAttr =
            if stacked then
                [ stackedA ]

            else
                []

        attrs =
            [ ids |> List.findIndex (equals selected) |> Maybe.withDefault 0 |> activeIndex
            , ((\id -> List.getAt id ids) >> change) |> onActivated
            ]

        viewTab { label, icon } =
            let
                ( iconAttr, iconNode ) =
                    case icon of
                        Just i ->
                            ( [ hasImageIcon ], [ Html.span [ HtmlA.slot "icon" ] [ i ] ] )

                        Nothing ->
                            ( [], [] )

                tabAttrs =
                    List.concat
                        [ [ HtmlA.label label ]
                        , iconAttr
                        , stackedAttr
                        ]
            in
            Html.node "mwc-tab" tabAttrs iconNode

        tabs =
            ids |> List.map (tab >> viewTab)
    in
    Html.node "mwc-tab-bar" attrs tabs



{- Private -}


stackedA : Html.Attribute msg
stackedA =
    HtmlA.attribute "stacked" ""


activeIndex : Int -> Html.Attribute msg
activeIndex =
    JsonE.int >> HtmlA.property "activeIndex"


hasImageIcon : Html.Attribute msg
hasImageIcon =
    HtmlA.attribute "hasImageIcon" ""


onActivated : (Int -> msg) -> Html.Attribute msg
onActivated wrap =
    JsonD.int
        |> JsonD.map wrap
        |> JsonD.at [ "target", "activeIndex" ]
        |> HtmlE.on "MDCTabBar:activated"
