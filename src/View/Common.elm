module View.Common exposing (..)

import Html exposing (Html)
import Html.Attributes
import Layout
import View.Style


nextPageButton : msg -> Html msg
nextPageButton onPress =
    Layout.textButton []
        { onPress = Just onPress
        , label = "Next Page"
        }


options : (String -> msg) -> List String -> Html msg
options onClick list =
    list
        |> List.map
            (\text ->
                Layout.textButton []
                    { onPress = onClick text |> Just
                    , label = text
                    }
            )
        |> Html.div
            [ Html.Attributes.style "display" "inline-flex"
            , View.Style.smallGap
            ]


placeholder : Html msg
placeholder =
    Html.div
        [ View.Style.placeholderBackground
        , View.Style.bordersRounded
        , Html.Attributes.style "display" "inline-box"
        ]
        []
