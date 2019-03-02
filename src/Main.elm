port module Main exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Events exposing (onClick)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
import V exposing (btn, cc, co, rr, t, tInt)


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
    co [ sans_serif, measure ]
        [ rr [ tc ]
            [ cc [] [ btn [ onClick Decrement ] [ t "-" ] ]
            , cc [] [ div [ classes [ pa3 ] ] [ tInt model ] ]
            , cc [] [ btn [ onClick Increment ] [ t "+" ] ]
            ]
        ]
