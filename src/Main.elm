port module Main exposing (main)

import Browser
import DnDList
import Html exposing (Html, div)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Html.Keyed
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


system : DnDList.System Msg String
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
                ( draggable, itemIds ) =
                    system.update msg model.draggable (model.items |> List.map (\item -> item.id))

                findItemWithId id =
                    model.items |> List.filter (\item -> item.id == id)

                newItems =
                    itemIds |> List.concatMap findItemWithId
            in
            ( { model | draggable = draggable, items = newItems }
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
        [ Html.Keyed.node "div"
            [ classes [ tc ] ]
            (model.items |> List.indexedMap (\idx item -> ( item.id, viewItem maybeDraggedIndex idx item )))
        , viewDraggedItem model.draggable model.items
        ]


viewItem : Maybe Int -> Int -> Item -> Html Msg
viewItem maybeDraggedIndex index item =
    let
        itemId : String
        itemId =
            "item-id-" ++ item.id
    in
    case maybeDraggedIndex of
        Nothing ->
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
                    ([ classes [ pa3, ba, br1, mv2, b__black_50 ]
                     ]
                        ++ system.dropEvents index
                    )
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
