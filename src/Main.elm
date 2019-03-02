port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


port fromJS : (Int -> msg) -> Sub msg


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init flags =
    ( 1212, Cmd.none )


subscriptions model =
    Sub.batch [ fromJS FromJs ]



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
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
