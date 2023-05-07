module View.Style exposing (..)

import Html exposing (Attribute)
import Html.Attributes


noBorder : Attribute msg
noBorder =
    Html.Attributes.style "border" "0"


bordersRounded : Attribute msg
bordersRounded =
    Html.Attributes.style "border-radius" "16px"


reviewBackground : Attribute msg
reviewBackground =
    Html.Attributes.style "background-color" "rgba(255,255,255,0.33)"


chipBackground : Attribute msg
chipBackground =
    Html.Attributes.style "background-color" "white"


buttonBackground : Attribute msg
buttonBackground =
    Html.Attributes.style "background-color" "#B7CC66"


appBackground : Attribute msg
appBackground =
    Html.Attributes.style "background-color" "#E4F5A3"


tinyGap : Attribute msg
tinyGap =
    Html.Attributes.style "gap" "0.25em"


smallGap : Attribute msg
smallGap =
    Html.Attributes.style "gap" "0.5em"


gap : Attribute msg
gap =
    Html.Attributes.style "gap" "1em"


bigGap : Attribute msg
bigGap =
    Html.Attributes.style "gap" "2em"


smallPadding : Attribute msg
smallPadding =
    Html.Attributes.style "padding" "0.5em"


padding : Attribute msg
padding =
    Html.Attributes.style "padding" "1em"


bigPadding : Attribute msg
bigPadding =
    Html.Attributes.style "padding" "2em"


bigPaddingX : List (Attribute msg)
bigPaddingX =
    [ Html.Attributes.style "padding-left" "2em"
    , Html.Attributes.style "padding-right" "2em"
    ]
