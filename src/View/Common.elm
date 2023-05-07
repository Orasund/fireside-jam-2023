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


options : (String -> msg) -> List ( String, Bool ) -> Html msg
options onClick list =
    list
        |> List.map
            (\( text, hasMore ) ->
                chipButton (onClick text)
                    (if hasMore then
                        text ++ "..."

                     else
                        text
                    )
            )
        |> Html.div
            [ Html.Attributes.style "display" "inline-flex"
            , Html.Attributes.style "flex-flow" "wrap"
            , View.Style.tinyGap
            ]


chipButton : msg -> String -> Html msg
chipButton onPress label =
    Layout.textButton
        [ View.Style.buttonBackground
        , View.Style.smallPadding
        , View.Style.bordersRounded
        , View.Style.noBorder
        ]
        { onPress = Just onPress
        , label = label
        }


boxButton : msg -> String -> Html msg
boxButton onPress label =
    Layout.textButton
        [ View.Style.buttonBackground
        , View.Style.smallPadding
        , View.Style.bordersRounded
        , View.Style.noBorder
        ]
        { onPress = Just onPress
        , label = label
        }
