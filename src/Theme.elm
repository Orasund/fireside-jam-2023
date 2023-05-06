module Theme exposing (..)


type alias ThemeId =
    String


type Theme
    = Wild
    | Domestic
    | Friendly
    | Dangerous
    | Cute


toString : Theme -> ThemeId
toString theme =
    case theme of
        Wild ->
            "Wild"

        Domestic ->
            "Domestic"

        Friendly ->
            "Friendly"

        Dangerous ->
            "Dangerous"

        Cute ->
            "Cute"
