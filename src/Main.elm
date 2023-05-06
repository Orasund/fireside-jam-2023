module Main exposing (main)

import Array exposing (Array)
import Browser
import Chapter exposing (Chapter)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Layout
import Semantics exposing (Semantics(..))
import View.Common
import View.Page
import View.Style


type alias Content =
    { text : String
    , options : Dict String (List Semantics)
    , remaining : List Semantics
    }


type alias Model =
    { chapter : Chapter
    , content : Dict String Content
    , remainingChapters : List Chapter
    }


type Msg
    = NextPage
    | PickOption { label : String, option : String }


init : () -> ( Model, Cmd Msg )
init () =
    ( { chapter = Chapter.Title
      , content = Dict.empty
      , remainingChapters = [ Chapter.Title ]
      }
        |> nextPage
    , Cmd.none
    )


view : Model -> Html Msg
view model =
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
                                }
                            )
            in
            { model
                | chapter = head
                , content =
                    content
                        |> Dict.map (\_ -> normalizeContent)
                , remainingChapters = tail
            }

        [] ->
            model


normalizeContent : Content -> Content
normalizeContent content =
    case content.remaining of
        head :: tail ->
            case head of
                Choose options ->
                    { content
                        | options = options |> Dict.fromList
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
                Just semantics ->
                    { content
                        | remaining = Semantics.Text args.option :: semantics ++ content.remaining
                        , options = Dict.empty
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
