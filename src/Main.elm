port module Main exposing (main)

import Browser
import DnDList
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
    { items : List Item, draggable : DnDList.Draggable }


type alias Flags =
    { items : List Item }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { items = flags.items, draggable = system.draggable }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ fromJs FromJs ]



-- SYSTEM


config : DnDList.Config Msg
config =
    { message = DndMsgReceived
    , movement = DnDList.Free
    }


system : DnDList.System Msg Item
system =
    DnDList.create config



-- UPDATE


type Msg
    = FromJs Int
    | DndMsgReceived DnDList.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        FromJs int ->
            ( model, Cmd.none )

        DndMsgReceived msg ->
            let
                ( draggable, items ) =
                    system.update msg model.draggable model.items
            in
            ( { model | draggable = draggable, items = items }
            , system.commands model.draggable
            )



-- VIEW


view : Model -> Html Msg
view model =
    co [ sans_serif, measure ]
        [ co [ tc ]
            (model.items |> List.map viewItem)
        ]


viewItem : Item -> Html Msg
viewItem item =
    rr [ pa3, ba, br1, mv2, b__black_50 ] [ t <| item.title ]
