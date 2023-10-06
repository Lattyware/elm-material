module Material.Util.Item exposing
    ( attrs
    , end
    , overline
    , start
    , supportingText
    , trailingSupportingText
    , view
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Material.Util as Util


type alias Pipeline a msg =
    { a
        | headline : List (Html msg)
        , disabled : Bool
        , overline : Maybe (List (Html msg))
        , supportingText : Maybe (List (Html msg))
        , trailingSupportingText : Maybe (List (Html msg))
        , start : Maybe (List (Html msg))
        , end : Maybe (List (Html msg))
        , attrs : List (Html.Attribute msg)
    }


start : List (Html msg) -> Pipeline a msg -> Pipeline a msg
start startElement pipeline =
    { pipeline | start = Just startElement }


overline : List (Html msg) -> Pipeline a msg -> Pipeline a msg
overline overlineElement pipeline =
    { pipeline | overline = Just overlineElement }


supportingText : List (Html msg) -> Pipeline a msg -> Pipeline a msg
supportingText supportingTextElement pipeline =
    { pipeline | supportingText = Just supportingTextElement }


trailingSupportingText : List (Html msg) -> Pipeline a msg -> Pipeline a msg
trailingSupportingText trailingSupportingTextElement pipeline =
    { pipeline | trailingSupportingText = Just trailingSupportingTextElement }


end : List (Html msg) -> Pipeline a msg -> Pipeline a msg
end endElement pipeline =
    { pipeline | end = Just endElement }


attrs : List (Html.Attribute msg) -> Pipeline a msg -> Pipeline a msg
attrs newAttrs pipeline =
    { pipeline | attrs = List.append pipeline.attrs newAttrs }


view : String -> List (Html.Attribute msg) -> List (Html msg) -> Pipeline a msg -> Html msg
view node otherAttrs otherContent pipeline =
    let
        attrsForItem =
            [ pipeline.disabled |> Util.boolAttr "disabled"
            , otherAttrs
            ]

        contents =
            [ pipeline.start |> Util.asSlot "start"
            , pipeline.overline |> Util.asSlot "overline"
            , [ Html.div [ HtmlA.attribute "slot" "headline" ] pipeline.headline ]
            , pipeline.supportingText |> Util.asSlot "supporting-text"
            , pipeline.trailingSupportingText |> Util.asSlot "trailing-supporting-text"
            , pipeline.end |> Util.asSlot "end"
            , otherContent
            ]
    in
    Html.node node
        (List.concat attrsForItem)
        (List.concat contents)
