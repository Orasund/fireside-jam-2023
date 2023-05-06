module View.Page exposing (..)

import Chapter exposing (Chapter(..))
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Layout
import Theme exposing (ThemeId)
import View.Common
import View.Style


fromChapter : ({ label : String, options : List String } -> Html msg) -> Dict String ( String, List String ) -> Chapter -> Html msg
fromChapter viewOptions dict chapter =
    let
        viewContent label =
            case dict |> Dict.get label of
                Just ( content, [] ) ->
                    [ Html.text content ]

                Just ( content, options ) ->
                    [ Html.text content
                    , viewOptions { label = label, options = options }
                    ]

                Nothing ->
                    [ View.Common.placeholder ]
    in
    case chapter of
        Title ->
            [ viewContent "Title" |> Html.h1 []
            , Html.p []
                [ Html.text "and other useful tips"
                , [ [ Html.text "by " ]
                  , viewContent "Author"
                  ]
                    |> List.concat
                    |> Html.p []
                ]
            ]
                |> Layout.column []


result : Dict ThemeId Int -> Html msg
result scores =
    scores
        |> Dict.toList
        |> List.map
            (\( themeId, score ) ->
                [ themeId |> Layout.text []
                , score |> String.fromInt |> Layout.text []
                ]
                    |> Layout.row [ Layout.contentWithSpaceBetween ]
            )
        |> Layout.column
            [ Html.Attributes.style "width" "100%"
            ]


toHtml : List (Html msg) -> Html msg
toHtml content =
    content
        |> Layout.column
            [ Html.Attributes.style "height" "600px"
            , Html.Attributes.style "width" "400px"
            , View.Style.padding
            , Layout.contentWithSpaceBetween
            , Layout.alignAtCenter
            ]
        |> Layout.el Layout.centered
