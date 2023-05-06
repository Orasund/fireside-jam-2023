module View.Review exposing (..)

import Config
import Html exposing (Html)
import Html.Attributes
import Layout
import Theme exposing (Theme(..))
import View.Style


fromTheme : Int -> Theme -> Html msg
fromTheme score theme =
    let
        text =
            case theme of
                Wild ->
                    if score < Config.maxScore then
                        "I felt alive reading it."

                    else
                        "With this guide, even house cats can survive in the wilderness."

                Funny ->
                    if score < Config.maxScore then
                        "Every cat should read this."

                    else
                        "My life completely changed after reading this guide."

                Dangerous ->
                    if score < Config.maxScore then
                        "Kittens, don't try this at home."

                    else
                        "This guide shows you how to be the coolest cat in town."

                Cute ->
                    if score < Config.maxScore then
                        "It's soo cute, I love it."

                    else
                        "My kittens love it so much. It's my favorite bedtime story."
    in
    Html.text text
        |> Layout.el
            [ View.Style.reviewBackground
            , View.Style.bordersRounded
            , View.Style.padding
            , Html.Attributes.style "width" "100%"
            ]
        |> Layout.withStack []
            [ \attrs ->
                (if score < Config.maxScore then
                    "⭐️"

                 else if score < Config.maxScore * 2 then
                    "⭐️⭐️"

                 else
                    "⭐️⭐️⭐️"
                )
                    |> Layout.text
                        ([ Html.Attributes.style "right" "1em"
                         , Html.Attributes.style "bottom" "-1em"
                         , View.Style.chipBackground
                         , View.Style.bordersRounded
                         , View.Style.smallPadding
                         ]
                            ++ attrs
                        )
            ]
