module Theme exposing (..)


type alias ThemeId =
    String


type Theme
    = Wild
    | Funny
    | Dangerous
    | Cute


values : List Theme
values =
    [ Wild, Funny, Dangerous, Cute ]


toString : Theme -> ThemeId
toString theme =
    case theme of
        Wild ->
            "Wild"

        Funny ->
            "Funny"

        Dangerous ->
            "Dangerous"

        Cute ->
            "Cute"


toEmoji : Theme -> String
toEmoji theme =
    case theme of
        Wild ->
            "😼"

        Funny ->
            "😹"

        Dangerous ->
            "🙀"

        Cute ->
            "😻"
