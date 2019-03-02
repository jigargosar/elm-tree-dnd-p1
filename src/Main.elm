port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events exposing (onKeyDown)
import DnDList
import Html exposing (Html, div)
import Html.Attributes exposing (id, tabindex)
import Html.Events exposing (onBlur, onClick, onFocus)
import Html.Keyed
import ItemTree exposing (Item, ItemTree)
import Json.Decode exposing (Decoder)
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
import Task
import V exposing (btn, cc, co, rr, t, tInt)


port fromJs : (Int -> msg) -> Sub msg


port toJsCache : { items : List Item } -> Cmd msg


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { itemTree : ItemTree
    , draggable : DnDList.Draggable
    , maybeFocusedItemId : Maybe String
    }


type alias Flags =
    { items : List Item }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { itemTree = ItemTree.fromList flags.items
      , draggable = system.draggable
      , maybeFocusedItemId = Nothing
      }
    , Cmd.none
    )


getItems model =
    model.itemTree |> ItemTree.toList


getItemById id model =
    model.itemTree |> ItemTree.getById id



-- SUBSCRIPTIONS


type alias KeyEvent =
    { key : String
    , ctrl : Bool
    , meta : Bool
    }


keyEventDecoder : Decoder KeyEvent
keyEventDecoder =
    Json.Decode.map3 KeyEvent
        (Json.Decode.at [ "key" ] Json.Decode.string)
        (Json.Decode.at [ "ctrl" ] Json.Decode.bool)
        (Json.Decode.at [ "meta" ] Json.Decode.bool)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fromJs FromJs
        , system.subscriptions model.draggable
        , onKeyDown <| Json.Decode.map KeyDownReceived keyEventDecoder
        ]



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
    | NOP
    | ItemReceivedFocus Item
    | ItemLostFocus Item
    | KeyDownReceived KeyEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NOP ->
            ( model, Cmd.none )

        FromJs int ->
            ( model, Cmd.none )

        DndMsgReceived msg ->
            let
                ( draggable, items ) =
                    system.update msg model.draggable (getItems model)

                maybeIdx =
                    system.draggedIndex model.draggable
            in
            ( { model | draggable = draggable, itemTree = ItemTree.fromList items }
            , Cmd.batch
                [ system.commands model.draggable
                , toJsCache { items = getItems model }
                , maybeIdx
                    |> Maybe.andThen (\idx -> getItems model |> List.drop idx |> List.head)
                    |> Maybe.map (getItemDomId >> Browser.Dom.focus >> Task.attempt (\_ -> NOP))
                    |> Maybe.withDefault Cmd.none
                ]
            )

        ItemReceivedFocus item ->
            ( { model | maybeFocusedItemId = Just item.id }, Cmd.none )

        ItemLostFocus item ->
            let
                hadFocus =
                    Just item.id == model.maybeFocusedItemId

                newModel =
                    if hadFocus then
                        { model | maybeFocusedItemId = Nothing }

                    else
                        model
            in
            ( newModel, Cmd.none )

        KeyDownReceived keyEvent ->
            let
                _ =
                    Debug.log "KeyDownReceived" keyEvent

                maybeFocusedItem : Maybe Item
                maybeFocusedItem =
                    model.maybeFocusedItemId
                        |> Maybe.andThen (\id -> getItems model |> List.filter (\item -> item.id == id) |> List.head)
            in
            if keyEvent.meta then
                case keyEvent.key of
                    "ArrowLeft" ->
                        ( model, Cmd.none )

                    "ArrowRight" ->
                        ( model, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        maybeDraggedIndex : Maybe Int
        maybeDraggedIndex =
            system.draggedIndex model.draggable

        getItemKey item =
            case system.draggedIndex model.draggable of
                Just _ ->
                    "dragging-" ++ item.id

                Nothing ->
                    item.id
    in
    co [ sans_serif, measure ]
        [ Html.Keyed.node "div"
            [ classes [] ]
            (List.indexedMap
                (\idx item -> ( getItemKey item, viewDraggableItem maybeDraggedIndex idx item ))
                (getItems model)
            )
        , viewDraggedItem model.draggable (getItems model)
        ]


viewItem attrs item =
    viewItemWithTitle attrs item.title


viewItemWithTitle attrs title =
    div
        (classes [ pa3, ba, br1, mv2, b__black_50 ]
            :: tabindex 1
            :: attrs
        )
        [ t <| title ]


getItemDomId item =
    "item-id-" ++ item.id


viewDraggableItem : Maybe Int -> Int -> Item -> Html Msg
viewDraggableItem maybeDraggedIndex index item =
    case maybeDraggedIndex of
        Nothing ->
            let
                itemDomId : String
                itemDomId =
                    getItemDomId item
            in
            div
                [ id itemDomId
                , classes [ flex, items_center, pa3, ba, br1, mv2, b__black_50 ]
                , tabindex 0
                , onFocus <| ItemReceivedFocus item
                , onBlur <| ItemLostFocus item
                ]
                [ div [ classes [ flex_grow_1 ] ] [ t item.title ]
                , div (classes [ "move" ] :: system.dragEvents index itemDomId) [ t "|||" ]
                ]

        Just draggedIndex ->
            if draggedIndex /= index then
                viewItem (system.dropEvents index) item

            else
                viewItemWithTitle [] "[---------]"


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
            div (system.draggedStyles draggable)
                [ viewItem [ classes [ bg_white, o_80 ] ] item
                ]

        Nothing ->
            Html.text ""
