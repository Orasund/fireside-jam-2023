module View.Page exposing (..)

import Chapter exposing (Chapter(..))
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Layout
import Theme exposing (Theme, ThemeId)
import View.Common
import View.Style


fromChapter : ({ label : String, option : String } -> msg) -> Dict String ( String, List String, List Theme ) -> Chapter -> Html msg
fromChapter onClick dict chapter =
    case chapter of
        Title ->
            [ viewContent
                { label = "Title"
                , onClick = onClick
                , content = dict
                }
                (Html.h1 [])
            , [ Layout.text [ Layout.contentCentered ] "by"
              , viewContent
                    { label = "Author"
                    , onClick = onClick
                    , content = dict
                    }
                    (Layout.row [])
              ]
                |> Layout.column [ View.Style.smallGap, Html.Attributes.style "width" "100%" ]
            ]
                |> Layout.column [ Layout.alignAtCenter, Layout.contentWithSpaceBetween ]


viewContent :
    { label : String
    , onClick : { label : String, option : String } -> msg
    , content : Dict String ( String, List String, List Theme )
    }
    -> (List (Html msg) -> Html msg)
    -> Html msg
viewContent args fun =
    (case args.content |> Dict.get args.label of
        Just ( content, [], themes ) ->
            ( [ Html.text content ], themes )

        Just ( content, options, themes ) ->
            ( [ Html.text content
              , View.Common.options
                    (\option ->
                        args.onClick
                            { label = args.label, option = option }
                    )
                    options
              ]
            , themes
            )

        Nothing ->
            ( [], [] )
    )
        |> (\( content, themes ) ->
                fun content
                    |> Layout.el
                        [ View.Style.bigPadding
                        , Html.Attributes.style "width" "100%"
                        ]
                    |> Layout.withStack []
                        [ \attrs ->
                            themes
                                |> List.map Theme.toEmoji
                                |> List.map (Layout.text [])
                                |> Layout.column
                                    ([ Html.Attributes.style "right" "0"
                                     , Html.Attributes.style "top" "50%"
                                     , Html.Attributes.style "font-size" "1.5em"
                                     , View.Style.tinyGap
                                     ]
                                        ++ attrs
                                    )
                        ]
           )


result : List ( Theme, Int ) -> Html msg
result scores =
    scores
        |> List.map
            (\( theme, score ) ->
                [ Theme.toEmoji theme
                    ++ " "
                    ++ Theme.toString theme
                    |> Layout.text []
                , score |> String.fromInt |> Layout.text []
                ]
                    |> Layout.row
                        [ Layout.contentWithSpaceBetween
                        , Html.Attributes.style "width" "100%"
                        ]
            )
        |> Layout.column
            [ View.Style.bigPadding
            , Layout.alignAtCenter
            ]


toHtml : List (Html msg) -> Html msg
toHtml content =
    [ """
    button:hover {
        filter: brightness(90%);
    }

    button:active {
        filter: brightness(75%);
    }
    """
        |> Html.text
        |> List.singleton
        |> Html.node "style" []
    , content
        |> Layout.column
            [ View.Style.padding
            , Layout.contentWithSpaceBetween
            , View.Style.appBackground
            , Html.Attributes.style "height" "600px"
            , Html.Attributes.style "width" "400px"
            ]
        |> Layout.el Layout.centered
    ]
        |> Html.div
            [ Html.Attributes.style "width" "100%"
            , Html.Attributes.style "height" "100%"
            ]
