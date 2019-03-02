port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)


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


btn attrs =
    button (classes [ br1, bw1, ba, b__light_blue, ma1 ] :: attrs)


view : Model -> Html Msg
view model =
    div [ classes [ sans_serif ] ]
        [ btn [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , btn [ onClick Increment ] [ text "+" ]
        ]
