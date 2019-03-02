port module Main exposing (main)

import Browser
import DnDList
import Html exposing (Html, div)
import Html.Attributes exposing (id)
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
    Sub.batch [ fromJs FromJs, system.subscriptions model.draggable ]



-- SYSTEM


config : DnDList.Config Msg
config =
    { message = DndMsgReceived
    , movement = DnDList.Vertical
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
    let
        maybeDraggedIndex : Maybe Int
        maybeDraggedIndex =
            system.draggedIndex model.draggable
    in
    co [ sans_serif, measure ]
        [ co [ tc ]
            (model.items |> List.indexedMap (viewItem maybeDraggedIndex))
        , viewDraggedItem model.draggable model.items
        ]


viewItem : Maybe Int -> Int -> Item -> Html Msg
viewItem maybeDraggedIndex index item =
    case maybeDraggedIndex of
        Nothing ->
            let
                itemId : String
                itemId =
                    "id-" ++ item.id
            in
            div
                ([ id itemId
                 , classes [ pa3, ba, br1, mv2, b__black_50 ]
                 ]
                    ++ system.dragEvents index itemId
                )
                [ t <| item.title ]

        Just draggedIndex ->
            if draggedIndex /= index then
                div
                    [ classes [ pa3, ba, br1, mv2, b__black_50 ]
                    ]
                    [ t <| item.title ]

            else
                div
                    [ classes [ pa3, ba, br1, mv2, b__black_50 ]
                    ]
                    [ t <| "[---------]" ]


viewDraggedItem : DnDList.Draggable -> List Item -> Html.Html Msg
viewDraggedItem draggable items =
    let
        maybeDraggedItem : Maybe Item
        maybeDraggedItem =
            system.draggedIndex draggable
                |> Maybe.andThen (\index -> items |> List.drop index |> List.head)
    in
    case maybeDraggedItem of
        Just item ->
            div
                (system.draggedStyles draggable)
                [ div
                    [ classes [ pa3, ba, br1, mv2, b__black_50 ]
                    ]
                    [ t <| item.title ]
                ]

        Nothing ->
            Html.text ""
