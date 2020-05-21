module Material.Slider exposing
    ( Model
    , view
    )

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode
import Json.Encode as Json


type alias Model msg =
    { min : Int
    , max : Int
    , step : Int
    , value : Int
    , onChange : Maybe (Int -> msg)
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
    in
    Html.node "mwc-slider" [ min model.min, max model.max, step model.step, value model.value, markers, pin, action ] []


step : Int -> Html.Attribute msg
step =
    Json.int >> HtmlA.property "step"


min : Int -> Html.Attribute msg
min =
    Json.int >> HtmlA.property "min"


max : Int -> Html.Attribute msg
max =
    Json.int >> HtmlA.property "max"


markers : Html.Attribute msg
markers =
    True |> Json.bool |> HtmlA.property "markers"


pin : Html.Attribute msg
pin =
    True |> Json.bool |> HtmlA.property "pin"


value : Int -> Html.Attribute msg
value =
    Json.int >> HtmlA.property "value"


onChange : (Int -> msg) -> Html.Attribute msg
onChange wrap =
    Json.Decode.at [ "target", "value" ] Json.Decode.int |> Json.Decode.map wrap |> HtmlE.on "change"
