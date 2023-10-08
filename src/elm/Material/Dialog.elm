module Material.Dialog exposing
    ( Dialog, dialog, view
    , headline
    , alert
    , attrs
    )

{-| Dialogs provide important prompts in a user flow.


# Core

@docs Dialog, dialog, view


## Optional Customisation

@docs headline
@docs alert


## Other

@docs attrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Material.Util as Util


type Type
    = Alert


type alias Pipeline msg =
    { close : msg
    , open : Bool
    , headline : Maybe (List (Html msg))
    , content : List (Html msg)
    , actions : List (Html msg)
    , type_ : Maybe Type
    , attrs : List (Html.Attribute msg)
    }


{-| A dialog definition, which can be turned into HTML with
[`view`](#view).
-}
type Dialog msg
    = Dialog (Pipeline msg)


{-| Dialogs provide important prompts in a user flow.
-}
dialog : msg -> List (Html msg) -> List (Html msg) -> Bool -> Dialog msg
dialog close content actions open =
    Pipeline
        close
        open
        Nothing
        content
        actions
        Nothing
        []
        |> Dialog


{-| Add a headline to the dialog.
-}
headline : List (Html msg) -> Dialog msg -> Dialog msg
headline headlineElement (Dialog pipeline) =
    Dialog { pipeline | headline = Just headlineElement }


{-| Make the dialog an alert dialog, that need to communicate an important message and
requires a user's response.
-}
alert : Dialog msg -> Dialog msg
alert (Dialog pipeline) =
    Dialog { pipeline | type_ = Just Alert }


{-| Add custom attributes to the Dialog.
-}
attrs : List (Html.Attribute msg) -> Dialog msg -> Dialog msg
attrs newAttrs (Dialog pipeline) =
    Dialog { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Turn an [`Dialog` definition](#Dialog) into HTML.
-}
view : Dialog msg -> Html msg
view (Dialog pipeline) =
    let
        typeString type_ =
            case type_ of
                Alert ->
                    "alert"

        allAttrs =
            List.concat
                [ pipeline.attrs
                , pipeline.open |> Util.boolAttr "open"
                , pipeline.type_ |> Maybe.map typeString |> Util.asAttr "type"
                , [ HtmlE.on "close" (JsonD.succeed pipeline.close) ]
                ]

        allContent =
            List.concat
                [ pipeline.headline |> Util.asSlot "headline"
                , [ Html.div [ HtmlA.attribute "slot" "content" ] pipeline.content
                  , Html.div [ HtmlA.attribute "slot" "actions" ] pipeline.actions
                  ]
                ]
    in
    Html.node "md-dialog" allAttrs allContent
