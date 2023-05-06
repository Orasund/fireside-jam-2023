module View.Style exposing (..)

import Html exposing (Attribute)
import Html.Attributes


bordersRounded : Attribute msg
bordersRounded =
    Html.Attributes.style "border-radius" "16px"


placeholderBackground : Attribute msg
placeholderBackground =
    Html.Attributes.style "background-color" "gray"


smallGap : Attribute msg
smallGap =
    Html.Attributes.style "gap" "0.5em"


gap : Attribute msg
gap =
    Html.Attributes.style "gap" "1em"


padding : Attribute msg
padding =
    Html.Attributes.style "padding" "1em"
