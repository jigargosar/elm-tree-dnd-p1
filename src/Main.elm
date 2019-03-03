port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events exposing (onKeyDown)
import DnDList
import Html exposing (Html, button, div)
import Html.Attributes exposing (id, tabindex)
import Html.Events exposing (onBlur, onClick, onFocus)
import Html.Keyed
import ItemLookup exposing (Item, ItemLookup)
import Json.Decode exposing (Decoder)
import Maybe.Extra
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
import Task
import V exposing (btn, cc, co, noHtml, rr, t, tInt)
import ViewDndItemTree


port fromJs : (Int -> msg) -> Sub msg


port pouchItemsLoaded : (List Item -> msg) -> Sub msg


port pouchItemChanged : (Item -> msg) -> Sub msg


port toJsCache : { items : List Item, maybeFocusedItemId : Maybe String } -> Cmd msg


port bulkItemDocs : List Item -> Cmd msg


port newItemDoc : ( Item, Int ) -> Cmd msg



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


getRootItemsOrEmpty model =
    model.itemLookup |> ItemLookup.getRootItems |> Maybe.withDefault []


getDisplayRootItems model =
    case model.maybeDndItems of
        Just items ->
            items

        Nothing ->
            getRootItemsOrEmpty model


getItemById id model =
    ItemLookup.getById id model.itemLookup


getRootItem : Model -> Maybe Item
getRootItem model =
    ItemLookup.getRoot model.itemLookup



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
                Browser.Dom.focus (ViewDndItemTree.getItemDomId item)
                    |> Task.attempt (FocusItemResultReceived item)
            )
        |> Maybe.withDefault Cmd.none


cacheNewModel model =
    toJsCache { items = getItems model, maybeFocusedItemId = model.maybeFocusedItemId }


refocusItemCmd model =
    model.maybeFocusedItemId
        |> Maybe.andThen (\id -> getItemById id model)
        |> Maybe.Extra.orElseLazy (\_ -> getRootItemsOrEmpty model |> List.head)
        |> focusMaybeItemCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NOP ->
            ( model, Cmd.none )

        PouchItemChanged item ->
            let
                newModel =
                    { model
                        | itemLookup = ItemLookup.insertAll [ item ] model.itemLookup
                        , maybeDndItems = Nothing
                    }
            in
            ( newModel
            , cacheNewModel newModel
            )

        AddItemClicked ->
            getRootItem model
                |> Maybe.map
                    (\rootItem ->
                        ( model
                        , Cmd.batch
                            [ newItemDoc ( rootItem, List.length rootItem.childIds )
                            ]
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

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
            let
                _ =
                    Debug.log "MouseUpReceived" ()
            in
            case ( model.maybeDndItems, ItemLookup.getRoot model.itemLookup ) of
                ( Just items, Just root ) ->
                    let
                        newRootChildIds =
                            List.map .id items
                    in
                    if newRootChildIds /= root.childIds then
                        let
                            newRoot : Item
                            newRoot =
                                { root | childIds = newRootChildIds }

                            updatedItems =
                                [ newRoot ]

                            newModel =
                                { model
                                    | maybeDndItems = Nothing
                                    , itemLookup = ItemLookup.insertAll updatedItems model.itemLookup
                                }
                        in
                        ( newModel
                        , Cmd.batch
                            [ bulkItemDocs updatedItems
                            ]
                        )

                    else
                        ( model, Cmd.none )

                _ ->
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
            in
            if keyEvent.meta then
                case keyEvent.key of
                    "ArrowLeft" ->
                        ( model, Cmd.none )

                    "ArrowRight" ->
                        model.maybeFocusedItemId
                            |> Debug.log "maybeFocusedItemId"
                            |> Maybe.andThen
                                (\id ->
                                    ItemLookup.getPrevSibAndParentOf id model.itemLookup
                                        |> Debug.log "getPrevSibAndParentOf"
                                        |> Maybe.map
                                            (\( newParent, oldParent ) ->
                                                let
                                                    i1 =
                                                        { oldParent | childIds = List.filter ((/=) id) oldParent.childIds }

                                                    i2 =
                                                        { newParent | childIds = id :: newParent.childIds }
                                                in
                                                [ i1, i2 ]
                                            )
                                )
                            |> Maybe.map
                                (\uItems ->
                                    ( model, bulkItemDocs uItems )
                                )
                            |> Maybe.withDefault ( model, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    co [ sans_serif, measure ]
        [ div []
            [ button [ onClick AddItemClicked ] [ t "add new" ]
            ]
        , viewTree model
        , viewDndItemTree model
        ]


viewTree model =
    let
        mRoot : Maybe Item
        mRoot =
            getRootItem model

        getChildrenOfId : String -> List Item
        getChildrenOfId id =
            ItemLookup.getChildrenOfId id model.itemLookup
                |> Maybe.withDefault []

        getChildren : Item -> List Item
        getChildren item =
            getChildrenOfId item.id

        viewItemTitle : Item -> Html Msg
        viewItemTitle item =
            div [ classes [ mv2, pa3, ba, b__black_50, br1 ] ] [ t item.title ]

        viewChildren : Item -> Html Msg
        viewChildren item =
            div [] (List.map viewItem (getChildren item))

        viewItem : Item -> Html Msg
        viewItem item =
            div []
                [ viewItemTitle item
                , viewChildren item
                ]
    in
    mRoot
        |> Maybe.map viewChildren
        |> Maybe.withDefault noHtml


viewForest forest =
    div [] []


viewDndItemTree model =
    let
        viewConfig =
            { system = system, onFocusMsg = ItemReceivedFocus, onBlurMsg = ItemLostFocus }
    in
    div [ classes [ dn ] ]
        [ ViewDndItemTree.viewDndItemTree viewConfig
            (getDisplayRootItems model)
            model.draggable
        ]
