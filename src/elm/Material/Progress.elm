module Material.Progress exposing
    ( Progress, view
    , linear, circular
    , value
    , attrs
    )

{-| Progress indicators inform users about the status of ongoing processes,
such as loading an app or submitting a form.


# Core

@docs Progress, view


## Types

@docs linear, circular

@docs value


## Optional Customisation

@docs attrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA


type Type
    = Circular
    | Linear


type Completion
    = Determinate Float
    | Indeterminate


type alias Pipeline msg =
    { type_ : Type
    , completion : Completion
    , attrs : List (Html.Attribute msg)
    }


{-| A progress definition, which can be turned into HTML with
[`view`](#view).
-}
type Progress msg
    = Progress (Pipeline msg)


{-| A circular progress indicator.
-}
circular : Progress msg
circular =
    Pipeline Circular Indeterminate [] |> Progress


{-| A linear progress indicator.
-}
linear : Progress msg
linear =
    Pipeline Linear Indeterminate [] |> Progress


{-| Set a specific value of completion from 0-1 for the progress, where
0 is just started, 1 is complete.
-}
value : Float -> Progress msg -> Progress msg
value completion (Progress pipeline) =
    Progress { pipeline | completion = Determinate completion }


{-| Add custom attributes to the progress indicator.
-}
attrs : List (Html.Attribute msg) -> Progress msg -> Progress msg
attrs newAttrs (Progress pipeline) =
    Progress { pipeline | attrs = List.append pipeline.attrs newAttrs }


{-| Turn a [`Progress` definition](#Progress) into HTML.
-}
view : Progress msg -> Html msg
view (Progress pipeline) =
    let
        node =
            case pipeline.type_ of
                Linear ->
                    "md-linear-progress"

                Circular ->
                    "md-circular-progress"

        completion =
            case pipeline.completion of
                Determinate amount ->
                    HtmlA.attribute "value" (String.fromFloat amount)

                Indeterminate ->
                    HtmlA.attribute "indeterminate" ""

        allAttrs =
            completion :: pipeline.attrs
    in
    Html.node node allAttrs []
