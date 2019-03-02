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


type alias Item =
    { id : String
    , title : String
    }


type alias Model =
    { items : List Item }


type alias Flags =
    { items : List Item }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { items = flags.items }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ fromJs FromJs ]



-- UPDATE


type Msg
    = FromJs Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromJs int ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    co [ sans_serif, measure ]
        [ co [ tc ]
            (model.items |> List.map viewItem)
        ]


viewItem : Item -> Html Msg
viewItem item =
    rr [] [ cc [] [ div [ classes [ pa3 ] ] [ t <| item.title ] ] ]
