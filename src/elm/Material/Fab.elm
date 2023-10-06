module Material.Fab exposing
    ( Fab, view
    , small, medium, large
    , extended, iconlessExtended
    , lowered
    , surface, primary, secondary, tertiary
    , attrs
    )

{-| The Floating action button represents the most important action on a
screen. It puts key actions within reach.


# Core

@docs Fab, view


# FAB Variants


## Sizes

@docs small, medium, large


## Extended

@docs extended, iconlessExtended


# Optional Customisation


## Styles

@docs lowered


## Colors

@docs surface, primary, secondary, tertiary


## Other

@docs attrs

-}

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Material.Util as Util


type alias Pipeline msg =
    { icon : Maybe (List (Html msg))
    , lowered : Bool
    , variant : String
    , attrs : List (Html.Attribute msg)
    }


{-| A floating action button definition, which can be turned into HTML with
[`view`](#view).
-}
type Fab msg
    = Fab (Pipeline msg)


init : Maybe (List (Html msg)) -> String -> Maybe String -> Maybe String -> Maybe msg -> Fab msg
init icon size label description action =
    let
        initialAttrs =
            List.concat
                [ Util.asAttr "label" label
                , Util.asAttr "aria-label" description
                , [ HtmlA.attribute "size" size
                  , Util.actionAttr HtmlE.onClick action
                  ]
                ]
    in
    Pipeline icon False "surface" initialAttrs |> Fab


{-| Turn a [`Fab` definition](#Fab) into HTML.
-}
view : Fab msg -> Html msg
view (Fab pipeline) =
    Html.node "md-fab"
        (List.concat
            [ pipeline.attrs
            , pipeline.lowered |> Util.boolAttr "lowered"
            , [ HtmlA.attribute "variant" pipeline.variant ]
            ]
        )
        (pipeline.icon |> Util.asSlot "icon")


{-| A small FAB. Takes the icon and a descriptive string explaining what the
button does.
-}
small : List (Html msg) -> String -> Maybe msg -> Fab msg
small givenIcon description =
    init (Just givenIcon) "small" Nothing (Just description)


{-| A small FAB. Takes the icon and a descriptive string explaining what the
button does.
-}
medium : List (Html msg) -> String -> Maybe msg -> Fab msg
medium givenIcon description =
    init (Just givenIcon) "medium" Nothing (Just description)


{-| A small FAB. Takes the icon and a descriptive string explaining what the
button does.
-}
large : List (Html msg) -> String -> Maybe msg -> Fab msg
large givenIcon description =
    init (Just givenIcon) "large" Nothing (Just description)


{-| A FAB extended with a label for additional emphasis. Takes the icon and a
label.
-}
extended : List (Html msg) -> String -> Maybe msg -> Fab msg
extended givenIcon label =
    init (Just givenIcon) "extended" (Just label) Nothing


{-| An extended FAB can omit an icon. Takes the label.
-}
iconlessExtended : String -> Maybe msg -> Fab msg
iconlessExtended label =
    init Nothing "extended" (Just label) Nothing


{-| Set if the FAB should be displayed at a lower elevation.
-}
lowered : Bool -> Fab msg -> Fab msg
lowered newLowered (Fab pipeline) =
    Fab { pipeline | lowered = newLowered }


{-| Set the FAB to display with the surface colour (the default).
-}
surface : Fab msg -> Fab msg
surface (Fab pipeline) =
    Fab { pipeline | variant = "surface" }


{-| Set the FAB to display with the primary colour.
-}
primary : Fab msg -> Fab msg
primary (Fab pipeline) =
    Fab { pipeline | variant = "primary" }


{-| Set the FAB to display with the secondary colour.
-}
secondary : Fab msg -> Fab msg
secondary (Fab pipeline) =
    Fab { pipeline | variant = "secondary" }


{-| Set the FAB to display with the tertiary colour.
-}
tertiary : Fab msg -> Fab msg
tertiary (Fab pipeline) =
    Fab { pipeline | variant = "tertiary" }


{-| Add custom attributes to the FAB.
-}
attrs : List (Html.Attribute msg) -> Fab msg -> Fab msg
attrs newAttrs (Fab pipeline) =
    Fab { pipeline | attrs = List.append pipeline.attrs newAttrs }
