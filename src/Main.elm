module Main exposing (main)

import Basics
import Css
import Html.Styled exposing (div, text)
import Html.Styled.Attributes exposing (css)


main =
    Html.Styled.toUnstyled <| view


fdiv =
    (//)


add =
    (+)


sansSerif =
    Css.fontFamilies [ "-system-ui" ]


view =
    div [ css [] ] [ text "hi", text <| String.fromInt <| add 1 1 ]
