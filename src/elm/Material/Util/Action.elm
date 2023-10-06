module Material.Util.Action exposing
    ( Action(..)
    , attrs
    , attrsPreventDefault
    , button
    , disabled
    , link
    , pipelineAttrs
    , replacedLink
    )

import Html
import Html.Events as HtmlE
import Json.Decode as JsonD
import Material.Util as Util


type Action msg
    = Link String (Maybe String) (Maybe (String -> Maybe String -> msg))
    | Button (Maybe msg)


type alias ActionPipeline a msg =
    { a | action : Action msg }


disabled : ActionPipeline a msg -> ActionPipeline a msg
disabled pipeline =
    { pipeline | action = Button Nothing }


button : Maybe msg -> ActionPipeline a msg -> ActionPipeline a msg
button action pipeline =
    { pipeline | action = Button action }


link : String -> Maybe String -> ActionPipeline a msg -> ActionPipeline a msg
link href target pipeline =
    { pipeline | action = Link href target Nothing }


replacedLink : (String -> Maybe String -> msg) -> String -> Maybe String -> ActionPipeline a msg -> ActionPipeline a msg
replacedLink pushUrl href target pipeline =
    { pipeline | action = Link href target (Just pushUrl) }


attrs : Action msg -> List (Html.Attribute msg)
attrs action =
    case action of
        Link pushUrl href target ->
            Util.replacedLink pushUrl href target

        Button buttonAction ->
            [ buttonAction |> Util.actionAttr HtmlE.onClick ]


pipelineAttrs : ActionPipeline a msg -> List (Html.Attribute msg)
pipelineAttrs { action } =
    attrs action


attrsPreventDefault : Action msg -> List (Html.Attribute msg)
attrsPreventDefault action =
    let
        onClickPreventDefault msg =
            HtmlE.preventDefaultOn "click" (JsonD.succeed ( msg, True ))
    in
    case action of
        Link pushUrl href target ->
            Util.replacedLink pushUrl href target

        Button buttonAction ->
            [ buttonAction |> Util.actionAttr onClickPreventDefault ]
