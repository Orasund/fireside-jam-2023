module Main exposing (main)

import Browser
import Chapter exposing (Chapter)
import Dict exposing (Dict)
import Html exposing (Html)
import Layout
import Semantics exposing (Semantics(..))
import Theme exposing (Theme, ThemeId)
import View.Common
import View.Page


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
    }


type Msg
    = Restart
    | NextPage
    | PickOption { label : String, option : String }


init : () -> ( Model, Cmd Msg )
init () =
    ( { chapter = Chapter.Title
      , content = Dict.empty
      , remainingChapters = [ Chapter.Title ]
      , scores = Dict.empty
      , finished = False
      }
        |> nextPage
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    if model.finished then
        [ Html.text "Reviews" |> Layout.heading1 []
        , View.Page.result model.scores
        , Layout.textButton
            []
            { onPress = Restart |> Just
            , label = "Restart"
            }
        ]
            |> View.Page.toHtml

    else
        [ model.chapter
            |> View.Page.fromChapter
                (\args ->
                    View.Common.options
                        (\option -> PickOption { label = args.label, option = option })
                        args.options
                )
                (model.content
                    |> Dict.map
                        (\_ { text, options } ->
                            ( text, options |> Dict.keys )
                        )
                )
        , if model.content |> Dict.values |> List.all (\{ options } -> Dict.isEmpty options) then
            Layout.textButton []
                { onPress = NextPage |> Just
                , label = "Next Page"
                }

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


pickOption : { label : String, option : String } -> Model -> Model
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
    { model
        | content =
            model.content
                |> Dict.update args.label
                    (Maybe.map updateContent)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextPage ->
            ( nextPage model, Cmd.none )

        PickOption args ->
            ( pickOption args model, Cmd.none )

        Restart ->
            init ()


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
