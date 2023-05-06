module View.Style exposing (..)

import Html exposing (Attribute)
import Html.Attributes


bordersRounded : Attribute msg
bordersRounded =
    Html.Attributes.style "border-radius" "16px"


placeholderBackground : Attribute msg
placeholderBackground =
    Html.Attributes.style "background-color" "gray"


gap : Attribute msg
gap =
    Html.Attributes.style "gap" "1em"
