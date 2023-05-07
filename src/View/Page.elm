module View.Page exposing (..)

import Chapter exposing (Chapter(..))
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Layout
import Theme exposing (Theme)
import View.Common
import View.Style


fromChapter :
    { onClick : { label : String, option : String } -> msg
    , showCredits : msg
    }
    -> Dict String ( String, List ( String, Bool ), List Theme )
    -> Chapter
    -> Html msg
fromChapter args dict chapter =
    case chapter of
        Title ->
            [ viewContent []
                { label = "Title"
                , onClick = args.onClick
                , content = dict
                }
                (Html.h1 [])
            , [ Layout.text [ Layout.contentCentered ] "by"
              , viewContent []
                    { label = "Author"
                    , onClick = args.onClick
                    , content = dict
                    }
                    (Layout.row [ Layout.contentCentered ])
              ]
                |> Layout.column [ View.Style.smallGap, Html.Attributes.style "width" "100%" ]
            , [ Html.text "A game by Lucas Payr (Orasund)"
              , Html.text "Credits" |> Layout.linkTo [ Html.Events.onClick args.showCredits ] "#"
              ]
                |> Layout.row
                    [ Html.Attributes.style "font-size" "0.75em"
                    , View.Style.padding
                    , View.Style.gap
                    ]
            ]
                |> Layout.column
                    [ Layout.alignAtCenter
                    , Layout.contentWithSpaceBetween
                    , Layout.fill
                    ]

        Quote ->
            [ viewContent []
                { label = "Quote"
                , onClick = args.onClick
                , content = dict
                }
                (Html.h2 [])
            , viewContent []
                { label = "Author"
                , onClick = args.onClick
                , content = dict
                }
                (\list ->
                    list
                        |> (::) (Html.text " - ")
                        |> Layout.row [ Html.Attributes.style "width" "100%", Html.Attributes.style "text-align" "right" ]
                )
            ]
                |> Layout.column [ Layout.alignAtCenter, Layout.contentWithSpaceBetween ]

        Rules ->
            [ Html.text "The golden rules" |> Layout.heading3 []
            , [ viewContent [ Layout.fill ]
                    { label = "1."
                    , onClick = args.onClick
                    , content = dict
                    }
                    (Layout.row [ Layout.alignAtBaseline, Html.Attributes.style "width" "100%" ])
              , viewContent [ Layout.fill ]
                    { label = "2."
                    , onClick = args.onClick
                    , content = dict
                    }
                    (Layout.row [ Layout.alignAtBaseline, Html.Attributes.style "width" "100%" ])
              , viewContent [ Layout.fill ]
                    { label = "3."
                    , onClick = args.onClick
                    , content = dict
                    }
                    (Layout.row [ Layout.alignAtBaseline, Html.Attributes.style "width" "100%" ])
              ]
                |> Layout.column
                    [ View.Style.bigGap
                    , Layout.fill
                    ]
            ]
                |> Layout.column [ Layout.contentWithSpaceBetween, Html.Attributes.style "height" "100%" ]


viewContent :
    List (Attribute msg)
    ->
        { label : String
        , onClick : { label : String, option : String } -> msg
        , content : Dict String ( String, List ( String, Bool ), List Theme )
        }
    -> (List (Html msg) -> Html msg)
    -> Html msg
viewContent attrs args fun =
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
                    |> Layout.withStack (attrs ++ View.Style.bigPaddingX)
                        [ \a ->
                            themes
                                |> List.map Theme.toEmoji
                                |> List.map (Layout.text [])
                                |> Layout.column
                                    ([ Html.Attributes.style "right" "0"
                                     , Html.Attributes.style "top" "0"
                                     , Html.Attributes.style "font-size" "1.5em"
                                     , View.Style.tinyGap
                                     ]
                                        ++ a
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
            , View.Style.smallGap
            , Layout.alignAtCenter
            ]


toHtml : List (Html msg) -> Html msg
toHtml content =
    [ """
    @font-face {
        font-family: "NotoEmojiColor";
        src: url("assets/NotoEmojiColor.ttf");
    }

    :root,body {
        font-family: serif,"NotoEmojiColor";
    }

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
            [ View.Style.bigPadding
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
