module Semantics exposing (..)

import Chapter exposing (Chapter)
import Dict exposing (Dict)
import Theme exposing (Theme(..))


type Semantics
    = Text String
    | Choose (List ( String, List Semantics, List Theme ))


choose : List ( String, List Semantics ) -> Semantics
choose list =
    list
        |> List.map (\( label, semantics ) -> ( label, semantics, [] ))
        |> Choose


enum : List String -> Semantics
enum list =
    list
        |> List.map (\label -> ( label, [], [] ))
        |> Choose


themedEnum : List ( String, List Theme ) -> Semantics
themedEnum list =
    list
        |> List.map (\( label, themes ) -> ( label, [], themes ))
        |> Choose


withText : String -> Semantics -> Semantics
withText text semantics =
    case semantics of
        Text text2 ->
            text2 ++ text |> Text

        Choose list ->
            list
                |> List.map (\( label, content, themes ) -> ( label ++ text, content, themes ))
                |> Choose


fromChapter : Chapter -> Dict String (List Semantics)
fromChapter chapter =
    case chapter of
        Chapter.Title ->
            let
                title =
                    [ Text "How to "
                    , choose
                        [ ( "be a "
                          , [ themedEnum
                                [ ( "good", [ Domestic ] )
                                , ( "bad", [ Dangerous, Wild ] )
                                , ( "cute", [ Friendly ] )
                                , ( "funny", [ Friendly ] )
                                ]
                                |> withText " cat"
                            ]
                          )
                        , ( "catch "
                          , [ themedEnum
                                [ ( "fish", [ Wild ] )
                                , ( "mice", [ Domestic ] )
                                , ( "birds", [ Wild ] )
                                ]
                            ]
                          )
                        , ( "do nothing", [] )
                        ]
                    , Text " "
                    , choose
                        [ ( "without "
                          , [ enum
                                [ "trying"
                                , "loosing a sweat"
                                , "looking"
                                ]
                            ]
                          )
                        , ( "with only one paw", [] )
                        , ( "and look stunning doing so", [] )
                        ]
                    ]

                author =
                    [ choose
                        [ ( "your "
                          , [ themedEnum
                                [ ( "friendly", [ Domestic, Friendly ] )
                                , ( "naughty", [ Dangerous, Wild ] )
                                ]
                            , Text " "
                            , themedEnum
                                [ ( "neighborhood cat", [ Dangerous ] )
                                , ( "house cat", [ Domestic ] )
                                , ( "wild cat", [ Wild ] )
                                ]
                            ]
                          )
                        , ( "the owner of a "
                          , [ themedEnum
                                [ ( "witch", [ Wild, Dangerous ] )
                                , ( "real", [] )
                                , ( "teenage", [ Dangerous ] )
                                , ( "unsure", [ Wild ] )
                                , ( "small", [ Friendly ] )
                                , ( "big", [] )
                                ]
                            , Text " human"
                            ]
                          )
                        ]
                    ]
            in
            [ ( "Title", title )
            , ( "Author", author )
            ]
                |> Dict.fromList
