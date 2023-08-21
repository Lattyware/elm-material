module Material.Slider exposing
    ( Model
    , view
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as JsonD
import Json.Encode as JsonE


type alias Model msg =
    { min : Int
    , max : Int
    , step : Int
    , value : Int
    , onChange : Maybe (Int -> msg)
    , attrs : List (Html.Attribute msg)
    }


view : Model msg -> Html msg
view model =
    let
        action =
            case model.onChange of
                Just wrap ->
                    onChange wrap

                Nothing ->
                    HtmlA.disabled True

        attrs =
            min model.min
                :: max model.max
                :: step model.step
                :: value model.value
                :: markers
                :: pin
                :: action
                :: model.attrs
    in
    Html.node "mwc-slider" attrs []


step : Int -> Html.Attribute msg
step =
    JsonE.int >> HtmlA.property "step"


min : Int -> Html.Attribute msg
min =
    JsonE.int >> HtmlA.property "min"


max : Int -> Html.Attribute msg
max =
    JsonE.int >> HtmlA.property "max"


markers : Html.Attribute msg
markers =
    True |> JsonE.bool |> HtmlA.property "markers"


pin : Html.Attribute msg
pin =
    True |> JsonE.bool |> HtmlA.property "pin"


value : Int -> Html.Attribute msg
value =
    JsonE.int >> HtmlA.property "value"


onChange : (Int -> msg) -> Html.Attribute msg
onChange wrap =
    JsonD.at [ "target", "value" ] JsonD.int
        |> JsonD.map wrap
        |> HtmlE.on "change"
