port module Main exposing (main)

import Browser
import Chapter exposing (Chapter)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Layout
import Maybe
import Semantics exposing (Semantics(..))
import Theme exposing (Theme, ThemeId)
import View.Common
import View.Page
import View.Review
import View.Style


port playSound : String -> Cmd msg


port setVolume : Float -> Cmd msg


type alias Content =
    { text : String
    , options : Dict String ( List Semantics, List Theme )
    , remaining : List Semantics
    , themes : List Theme
    }


type alias Model =
    { chapter : Chapter
    , content : Dict String Content
    , remainingChapters : List Chapter
    , scores : Dict ThemeId Int
    , finished : Bool
    , showCredits : Bool
    }


type Msg
    = Restart
    | NextPage
    | PickOption { label : String, option : String }
    | SetVolume Float
    | ToggleShowingCredits


init : () -> ( Model, Cmd Msg )
init () =
    ( { chapter = Chapter.Title
      , content = Dict.empty
      , remainingChapters = Chapter.values
      , scores = Dict.empty
      , finished = False
      , showCredits = False
      }
        |> nextPage
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    if model.showCredits then
        [ Html.text "Credits" |> Layout.heading1 []
        , Html.text "Noto Emoji Font by Google"
            |> Layout.linkToNewTab [] "https://fonts.google.com/noto/specimen/Noto+Color+Emoji"
        , View.Common.boxButton ToggleShowingCredits "Back"
        ]
            |> View.Page.toHtml

    else if model.finished then
        [ Html.text "Reviews" |> Layout.heading1 []
        , Theme.values
            |> List.filterMap
                (\theme ->
                    model.scores
                        |> Dict.get (Theme.toString theme)
                        |> Maybe.map
                            (\score ->
                                View.Review.fromTheme score theme
                            )
                )
            |> Layout.column
                [ View.Style.gap
                , Html.Attributes.style "width" "100%"
                ]
        , Theme.values
            |> List.filterMap
                (\theme ->
                    model.scores
                        |> Dict.get (Theme.toString theme)
                        |> Maybe.map (Tuple.pair theme)
                )
            |> View.Page.result
        , View.Common.boxButton Restart "Restart"
        ]
            |> View.Page.toHtml

    else
        [ model.chapter
            |> View.Page.fromChapter
                { onClick = PickOption
                , showCredits = ToggleShowingCredits
                }
                (model.content
                    |> Dict.map
                        (\_ { text, options, themes } ->
                            ( text
                            , options
                                |> Dict.map
                                    (\_ ( semantics, _ ) ->
                                        semantics /= []
                                    )
                                |> Dict.toList
                            , themes
                            )
                        )
                )
        , if model.content |> Dict.values |> List.all (\{ options } -> Dict.isEmpty options) then
            View.Common.boxButton NextPage "Next Page"

          else
            Layout.text [] "Finish the sentences."
        ]
            |> View.Page.toHtml


nextPage : Model -> Model
nextPage model =
    let
        scores =
            model.content
                |> Dict.values
                |> List.concatMap .themes
                |> List.map Theme.toString
                |> List.foldl
                    (\id ->
                        Dict.update id
                            (\maybe ->
                                maybe
                                    |> Maybe.withDefault 0
                                    |> (+) 1
                                    |> Just
                            )
                    )
                    model.scores
    in
    case model.remainingChapters of
        head :: tail ->
            let
                content =
                    Semantics.fromChapter head
                        |> Dict.map
                            (\_ remaining ->
                                { text = ""
                                , options = Dict.empty
                                , remaining = remaining
                                , themes = []
                                }
                            )
            in
            { model
                | chapter = head
                , content =
                    content
                        |> Dict.map (\_ -> normalizeContent)
                , remainingChapters = tail
                , scores = scores
            }

        [] ->
            { model
                | scores = scores
                , finished = True
            }


normalizeContent : Content -> Content
normalizeContent content =
    case content.remaining of
        head :: tail ->
            case head of
                Choose options ->
                    { content
                        | options =
                            options
                                |> List.map
                                    (\( label, semantic, theme ) ->
                                        ( label, ( semantic, theme ) )
                                    )
                                |> Dict.fromList
                        , remaining = tail
                    }

                Text string ->
                    { content
                        | text = content.text ++ string
                        , remaining = tail
                    }
                        |> normalizeContent

        [] ->
            content


pickOption : { label : String, option : String } -> Model -> ( Model, Cmd msg )
pickOption args model =
    let
        updateContent content =
            case content.options |> Dict.get args.option of
                Just ( semantics, themes ) ->
                    { content
                        | remaining = Semantics.Text args.option :: semantics ++ content.remaining
                        , options = Dict.empty
                        , themes = content.themes ++ themes
                    }
                        |> normalizeContent

                Nothing ->
                    content
    in
    model.content
        |> Dict.get args.label
        |> Maybe.map updateContent
        |> Maybe.map
            (\content ->
                ( { model
                    | content = Dict.insert args.label content model.content
                  }
                , if content.options |> Dict.isEmpty then
                    playSound "cat"

                  else
                    Cmd.none
                )
            )
        |> Maybe.withDefault ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextPage ->
            ( nextPage model, playSound "curiousCat" )

        PickOption args ->
            pickOption args model
                |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, playSound "buttonUp" ])

        Restart ->
            init ()
                |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, playSound "curiousCat" ])

        SetVolume amount ->
            ( model, setVolume amount )

        ToggleShowingCredits ->
            ( { model | showCredits = not model.showCredits }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
