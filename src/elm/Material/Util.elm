module Material.Util exposing
    ( actionAttr
    , argActionAttr
    , asAttr
    , asSlot
    , boolAttr
    , onActionAttr
    , replacedLink
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Json.Decode.Pipeline as JsonD


boolAttr : String -> Bool -> List (Html.Attribute msg)
boolAttr attribute present =
    if present then
        [ HtmlA.attribute attribute "" ]

    else
        []


asAttr : String -> Maybe String -> List (Html.Attribute msg)
asAttr attribute maybeValue =
    case maybeValue of
        Just value ->
            [ HtmlA.attribute attribute value ]

        Nothing ->
            []


asSlot : String -> Maybe (Html msg) -> List (Html msg)
asSlot slot maybeElement =
    case maybeElement of
        Just element ->
            [ Html.div [ HtmlA.attribute "slot" slot ] [ element ] ]

        Nothing ->
            []


actionAttr : (msg -> Html.Attribute msg) -> Maybe msg -> Html.Attribute msg
actionAttr onEvent maybeAction =
    case maybeAction of
        Just action ->
            onEvent action

        Nothing ->
            HtmlA.disabled True


argActionAttr : ((arg -> msg) -> Html.Attribute msg) -> Maybe (arg -> msg) -> Html.Attribute msg
argActionAttr onEvent maybeAction =
    case maybeAction of
        Just action ->
            onEvent action

        Nothing ->
            HtmlA.disabled True


onActionAttr : String -> JsonD.Decoder arg -> Maybe (arg -> msg) -> Html.Attribute msg
onActionAttr event decoder maybeAction =
    case maybeAction of
        Just action ->
            HtmlE.on event (decoder |> JsonD.map action)

        Nothing ->
            HtmlA.disabled True


{-| Unfortunately Elm doesn't handle links in web components
gracefully, so this hack stops them from working normally and does a message
where we can `pushUrl` instead, as appropriate.
-}
replacedLink : String -> Maybe String -> Maybe (String -> Maybe String -> msg) -> List (Html.Attribute msg)
replacedLink href target maybePushUrl =
    let
        hrefAndTarget =
            HtmlA.attribute "href" href :: asAttr "target" target
    in
    case maybePushUrl of
        Just pushUrl ->
            let
                replaceWithPush ctrlKey metaKey shiftKey button =
                    if ctrlKey || metaKey || shiftKey || button >= 1 then
                        JsonD.fail ""

                    else
                        JsonD.succeed ( pushUrl href target, True )

                decoder =
                    JsonD.succeed replaceWithPush
                        |> JsonD.optional "ctrlKey" JsonD.bool False
                        |> JsonD.optional "metaKey" JsonD.bool False
                        |> JsonD.optional "shiftKey" JsonD.bool False
                        |> JsonD.optional "button" JsonD.int 0
                        |> JsonD.andThen identity
            in
            HtmlE.preventDefaultOn "click" decoder :: hrefAndTarget

        Nothing ->
            hrefAndTarget
