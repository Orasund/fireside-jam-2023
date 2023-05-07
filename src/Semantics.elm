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


enum : List ( String, List Theme ) -> Semantics
enum list =
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


titleChapter : Dict String (List Semantics)
titleChapter =
    let
        title =
            [ Text "How to "
            , choose
                [ ( "be a "
                  , [ enum
                        [ ( "good", [ Cute ] )
                        , ( "bad", [ Dangerous, Wild ] )
                        , ( "cute", [ Cute ] )
                        , ( "funny", [ Funny ] )
                        ]
                        |> withText " cat"
                    ]
                  )
                , ( "catch "
                  , [ enum
                        [ ( "fish", [ Wild ] )
                        , ( "mice", [ Funny ] )
                        , ( "birds", [ Wild ] )
                        , ( "rats", [ Dangerous ] )
                        ]
                    ]
                  )
                , ( "survive "
                  , [ enum
                        [ ( "with a human", [ Funny ] )
                        , ( "in the woods", [ Wild ] )
                        , ( "on the streets", [ Dangerous ] )
                        ]
                    ]
                  )
                ]
            , Text " "
            , choose
                [ ( "without "
                  , [ enum
                        [ ( "help", [ Cute ] )
                        , ( "trying", [ Funny ] )
                        , ( "loosing a sweat", [ Dangerous ] )
                        ]
                    ]
                  )
                , ( ".", [] )
                ]
            ]

        author =
            [ choose
                [ ( "your "
                  , [ enum
                        [ ( "friendly", [ Cute, Funny ] )
                        , ( "naughty", [ Dangerous, Wild ] )
                        ]
                        |> withText " "
                    , enum
                        [ ( "neighborhood cat", [ Dangerous ] )
                        , ( "house cat", [ Funny ] )
                        , ( "wild cat", [ Wild ] )
                        ]
                    ]
                  )
                , ( "the owner of a "
                  , [ enum
                        [ ( "witch", [ Wild, Dangerous ] )
                        , ( "teenage", [ Dangerous ] )
                        , ( "unsure", [ Wild ] )
                        , ( "small", [ Funny ] )
                        ]
                        |> withText " human"
                    ]
                  )
                ]
            ]
    in
    [ ( "Title", title )
    , ( "Author", author )
    ]
        |> Dict.fromList


quoteChapter : Dict String (List Semantics)
quoteChapter =
    let
        quote =
            [ choose
                [ ( "A "
                  , [ enum
                        [ ( "mouse", [ Funny ] )
                        , ( "fish", [ Wild ] )
                        , ( "bird", [ Wild ] )
                        , ( "nap", [ Cute ] )
                        ]
                        |> withText " a day, "
                    , choose
                        [ ( "keeps the "
                          , [ enum
                                [ ( "human ok", [ Funny ] )
                                , ( "dog at bay", [ Dangerous ] )
                                , ( "vet away", [ Cute ] )
                                ]
                            ]
                          )
                        ]
                    ]
                  )
                , ( "A cat "
                  , [ enum
                        [ ( "that can climb has no need to beg or mime", [ Wild ] )
                        , ( "who naps without a care, has not a single worry or despair", [ Funny ] )
                        , ( "of gladness seldom falls into madness", [ Dangerous ] )
                        , ( "who cuddles is a loving soul, warming hearts with a purring roll", [ Cute ] )
                        ]
                    ]
                  )
                , ( "A good human and health "
                  , [ enum [ ( "are cat's best wealth", [ Cute, Funny ] ) ]
                    ]
                  )
                , ( "Wake up on all paws, "
                  , [ enum [ ( "and the day is yours", [ Dangerous, Wild ] ) ] ]
                  )
                ]
            ]

        author =
            [ choose
                [ ( "A cat I once "
                  , [ enum
                        [ ( "met", [] )
                        , ( "saw", [] )
                        , ( "followed", [] )
                        ]
                    , Text " "
                    , enum
                        [ ( "in the park", [] )
                        , ( "at night", [] )
                        , ( "in town", [] )
                        , ( "across the street", [] )
                        ]
                    ]
                  )
                , ( "Queen Elizapurrth", [] )
                , ( "Cleocatra", [] )
                , ( "Garfield", [] )
                , ( "Katy Purry", [] )
                , ( "Clawed Pitt", [] )
                , ( "William Shakespurr", [] )
                ]
            ]
    in
    [ ( "Quote", quote )
    , ( "Author", author )
    ]
        |> Dict.fromList


rulesChapter : Dict String (List Semantics)
rulesChapter =
    let
        first =
            [ Text "1. "
            , enum [ ( "Never", [] ), ( "Always", [] ) ]
                |> withText " go "
            , enum
                [ ( "outside", [ Wild ] )
                , ( "inside", [ Funny ] )
                , ( "to bed", [ Cute ] )
                , ( "hunting", [ Dangerous ] )
                ]
                |> withText " "
            , enum
                [ ( "before", [] )
                , ( "after", [] )
                ]
                |> withText " "
            , enum
                [ ( "sunrise", [ Funny ] )
                , ( "sunset", [ Dangerous ] )
                , ( "you cleaned your fur", [ Cute ] )
                , ( "you had a nap", [ Cute ] )
                , ( "you played with your human", [ Cute ] )
                ]
            ]

        second =
            [ Text "2. "
            , enum [ ( "Never", [] ), ( "Always", [] ) ]
                |> withText " trust "
            , enum
                [ ( "a dog", [ Wild ] )
                , ( "a cat", [ Dangerous ] )
                , ( "a human", [ Cute ] )
                ]
            , enum
                [ ( " you just met", [ Wild ] )
                , ( " that lives with you", [ Funny ] )
                , ( ".", [ Dangerous ] )
                , ( " that hasn't cuddles with you", [ Cute ] )
                ]
            ]

        third =
            [ Text "3. "
            , enum [ ( "Never", [] ), ( "Always", [] ) ]
                |> withText " eat "
            , enum
                [ ( "a fish", [ Wild ] )
                , ( "a bird", [ Dangerous ] )
                , ( "human food", [ Cute ] )
                , ( "a mouse", [ Funny ] )
                , ( "gras", [ Wild ] )
                , ( "catnip", [ Dangerous ] )
                ]
                |> withText " that "
            , enum
                [ ( "moves", [ Wild ] )
                , ( "a human gives to you", [ Cute ] )
                , ( "another cat gives to you", [ Dangerous ] )
                , ( "you had not smelled before", [ Funny ] )
                ]
            ]
    in
    [ ( "1.", first )
    , ( "2.", second )
    , ( "3.", third )
    ]
        |> Dict.fromList


fromChapter : Chapter -> Dict String (List Semantics)
fromChapter chapter =
    case chapter of
        Chapter.Title ->
            titleChapter

        Chapter.Quote ->
            quoteChapter

        Chapter.Rules ->
            rulesChapter
