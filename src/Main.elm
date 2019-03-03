port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events exposing (onKeyDown)
import DnDList
import Html exposing (Html, button, div)
import Html.Attributes exposing (id, tabindex)
import Html.Events exposing (onBlur, onClick, onFocus)
import Html.Keyed
import ItemLookup exposing (Item, ItemLookup)
import ItemTreeCursor exposing (ItemTreeCursor)
import Json.Decode exposing (Decoder)
import Maybe.Extra
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
import Task
import V exposing (btn, cc, co, rr, t, tInt)


port fromJs : (Int -> msg) -> Sub msg


port pouchItemsLoaded : (List Item -> msg) -> Sub msg


port pouchItemChanged : (Item -> msg) -> Sub msg


port toJsCache : { items : List Item, maybeFocusedItemId : Maybe String } -> Cmd msg


port bulkItemDocs : List Item -> Cmd msg


port newItemDoc : () -> Cmd msg



--port debouncedBulkItemDocs : List Item -> Cmd msg


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { itemLookup : ItemLookup
    , draggable : DnDList.Draggable
    , maybeFocusedItemId : Maybe String
    , maybeDndItems : Maybe (List Item)
    }


type alias Flags =
    { items : List Item, maybeFocusedItemId : Maybe String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    update InitReceived
        { itemLookup = ItemLookup.fromList flags.items
        , draggable = system.draggable
        , maybeFocusedItemId = flags.maybeFocusedItemId
        , maybeDndItems = Nothing
        }


getItems model =
    model.itemLookup |> ItemLookup.toList


getRootItems model =
    model.itemLookup |> ItemLookup.getRootItems


getDisplayRootItems model =
    case model.maybeDndItems of
        Just items ->
            items

        Nothing ->
            getRootItems model


getItemById id model =
    ItemLookup.getById id model.itemLookup



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
        (Json.Decode.at [ "ctrlKey" ] Json.Decode.bool)
        (Json.Decode.at [ "metaKey" ] Json.Decode.bool)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fromJs FromJs
        , pouchItemsLoaded PouchItemsLoaded
        , pouchItemChanged PouchItemChanged
        , system.subscriptions model.draggable
        , onKeyDown <| Json.Decode.map KeyDownReceived keyEventDecoder
        , Browser.Events.onMouseUp <| Json.Decode.succeed MouseUpReceived
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
    = NOP
    | AddItemClicked
    | FromJs Int
    | FocusItemResultReceived Item (Result Browser.Dom.Error ())
    | DndMsgReceived DnDList.Msg
    | ItemReceivedFocus Item
    | ItemLostFocus Item
    | KeyDownReceived KeyEvent
    | MouseUpReceived
    | InitReceived
    | PouchItemsLoaded (List Item)
    | PouchItemChanged Item


focusMaybeItemCmd maybeItem =
    maybeItem
        |> Maybe.map
            (\item ->
                Browser.Dom.focus (getItemDomId item)
                    |> Task.attempt (FocusItemResultReceived item)
            )
        |> Maybe.withDefault Cmd.none


cacheNewModel model =
    toJsCache { items = getItems model, maybeFocusedItemId = model.maybeFocusedItemId }


refocusItemCmd model =
    model.maybeFocusedItemId
        |> Maybe.andThen (\id -> getItemById id model)
        |> Maybe.Extra.orElseLazy (\_ -> getRootItems model |> List.head)
        |> focusMaybeItemCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NOP ->
            ( model, Cmd.none )

        PouchItemChanged item ->
            ( { model
                | itemLookup = ItemLookup.insertAll [ item ] model.itemLookup
                , maybeDndItems = Nothing
              }
            , Cmd.none
            )

        AddItemClicked ->
            ( model, Cmd.batch [ newItemDoc () ] )

        PouchItemsLoaded items ->
            let
                newModel =
                    { model
                        | itemLookup = ItemLookup.fromList items
                        , maybeDndItems = Nothing
                    }
            in
            ( newModel
            , Cmd.batch
                [ cacheNewModel newModel
                , refocusItemCmd newModel
                ]
            )

        FocusItemResultReceived item result ->
            case result of
                Err error ->
                    --                    let
                    --                        _ =
                    --                            Debug.log "FocusItemResultReceived Err" ( item, error )
                    --                    in
                    ( model, Cmd.none )

                Ok _ ->
                    ( model, Cmd.none )

        InitReceived ->
            ( model
            , Cmd.batch
                [ refocusItemCmd model
                ]
            )

        FromJs int ->
            ( model, Cmd.none )

        MouseUpReceived ->
            case model.maybeDndItems of
                Just items ->
                    let
                        _ =
                            Debug.log "MouseUpReceived" ()

                        updatedItems : List Item
                        updatedItems =
                            items
                                |> List.indexedMap
                                    (\idx item ->
                                        if item.rootIdx == idx then
                                            Nothing

                                        else
                                            Just { item | rootIdx = idx }
                                    )
                                |> List.filterMap identity
                                |> Debug.log "newRootItems"

                        newModel =
                            { model
                                | maybeDndItems = Nothing
                                , itemLookup = ItemLookup.insertAll updatedItems model.itemLookup
                            }
                    in
                    ( newModel
                    , Cmd.batch
                        [ bulkItemDocs updatedItems
                        , cacheNewModel newModel
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        DndMsgReceived msg ->
            let
                ( draggable, rootItems ) =
                    system.update msg model.draggable (getDisplayRootItems model)

                maybeIdx =
                    system.draggedIndex model.draggable
                        |> Debug.log "system.draggedIndex"
            in
            ( { model | draggable = draggable, maybeDndItems = Just rootItems }
            , Cmd.batch
                [ system.commands model.draggable
                , maybeIdx
                    |> Maybe.andThen (\idx -> rootItems |> List.drop idx |> List.head)
                    |> focusMaybeItemCmd
                ]
            )

        ItemReceivedFocus item ->
            let
                newModel =
                    { model | maybeFocusedItemId = Just item.id }
            in
            ( newModel, Cmd.batch [ cacheNewModel newModel ] )

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
            ( newModel, Cmd.batch [ cacheNewModel newModel ] )

        KeyDownReceived keyEvent ->
            let
                _ =
                    Debug.log "KeyDownReceived" keyEvent

                updateFocusedItemTreeCursor fn =
                    model.maybeFocusedItemId
                        |> Maybe.andThen (\id -> ItemTreeCursor.forId id model.itemLookup)
                        |> Maybe.andThen fn
                        |> Maybe.map
                            (\cursor ->
                                let
                                    newModel =
                                        { model | itemLookup = ItemTreeCursor.getItemLookup cursor }
                                in
                                ( newModel
                                , Cmd.batch
                                    [ cacheNewModel newModel
                                    ]
                                )
                            )
                        |> Maybe.withDefault ( model, Cmd.none )
            in
            if keyEvent.meta then
                case keyEvent.key of
                    "ArrowLeft" ->
                        ( model, Cmd.none )

                    "ArrowRight" ->
                        let
                            maybeItem =
                                model.maybeFocusedItemId
                                    |> Maybe.andThen (\id -> ItemLookup.getById id model.itemLookup)

                            maybePrevSibling =
                                model.maybeFocusedItemId
                                    |> Maybe.andThen (\id -> ItemLookup.getPrevSibling id model.itemLookup)
                                    |> Debug.log "getPrevSibling"

                            maybeParent =
                                model.maybeFocusedItemId
                                    |> Maybe.andThen (\id -> ItemLookup.getParentById id model.itemLookup)

                            _ =
                                case maybeParent of
                                    Just parent ->
                                        1

                                    Nothing ->
                                        2
                        in
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

        displayRootItems =
            getDisplayRootItems model
    in
    co [ sans_serif, measure ]
        [ div []
            [ button [ onClick AddItemClicked ] [ t "add new" ]
            ]
        , Html.Keyed.node "div"
            [ classes [] ]
            (List.indexedMap
                (\idx item -> ( getItemKey item, viewDraggableItem maybeDraggedIndex idx item ))
                displayRootItems
            )
        , viewDraggedItem model.draggable displayRootItems
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
