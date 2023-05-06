module View.Page exposing (..)

import Chapter exposing (Chapter(..))
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Layout
import View.Common


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


toHtml : List (Html msg) -> Html msg
toHtml content =
    content
        |> Layout.column
            ([ Html.Attributes.style "height" "100%"
             , Html.Attributes.style "width" "100%"
             ]
                ++ Layout.centered
            )
