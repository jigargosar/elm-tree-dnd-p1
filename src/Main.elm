port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (b__blue, ba, bn, br1)


port fromJs : (Int -> msg) -> Sub msg


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init flags =
    ( 1212, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ fromJs FromJs ]



-- UPDATE


type Msg
    = Increment
    | Decrement
    | FromJs Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none )

        FromJs int ->
            ( model + int, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ classes [ br1, ba, b__blue ], onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
