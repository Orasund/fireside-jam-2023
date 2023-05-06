module Semantics exposing (..)

import Chapter exposing (Chapter)
import Dict exposing (Dict)


type Semantics
    = Text String
    | Choose (List ( String, List Semantics ))


fromChapter : Chapter -> Dict String (List Semantics)
fromChapter chapter =
    case chapter of
        Chapter.Title ->
            let
                title =
                    [ Text "How to "
                    , Choose
                        [ ( "be a "
                          , [ Choose
                                [ ( "good", [] )
                                , ( "bad", [] )
                                , ( "cute", [] )
                                , ( "funny", [] )
                                ]
                            , Text " cat"
                            ]
                          )
                        , ( "catch "
                          , [ Choose
                                [ ( "fish", [] )
                                , ( "mice", [] )
                                , ( "birds", [] )
                                ]
                            ]
                          )
                        , ( "do nothing", [] )
                        ]
                    , Text " "
                    , Choose
                        [ ( "without "
                          , [ Choose
                                [ ( "trying", [] )
                                , ( "loosing a sweat", [] )
                                , ( "looking", [] )
                                ]
                            ]
                          )
                        , ( "with only one paw", [] )
                        , ( "and look stunning doing so", [] )
                        ]
                    ]

                author =
                    [ Choose
                        [ ( "your friendly neighbor", [] )
                        , ( "the owner of a "
                          , [ Choose
                                [ ( "witch", [] )
                                , ( "real", [] )
                                , ( "teenage", [] )
                                , ( "unsure", [] )
                                , ( "small", [] )
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
