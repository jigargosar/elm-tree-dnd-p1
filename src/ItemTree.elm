module ItemTree exposing (Item, ItemTree, fromList, getAncestorIds, getById, toArray, toList)

import Array exposing (Array)
import Dict exposing (Dict)


type alias Item =
    { id : String
    , title : String
    , pid : Maybe String
    }


type alias ItemTree =
    Dict String Item


fromList : List Item -> ItemTree
fromList itemList =
    itemList
        |> List.map (\item -> ( item.id, item ))
        |> Dict.fromList


toList : ItemTree -> List Item
toList itemTree =
    itemTree |> Dict.values


getById : String -> ItemTree -> Maybe Item
getById id itemTree =
    Dict.get id itemTree


getParentById : String -> ItemTree -> Maybe Item
getParentById id itemTree =
    getById id itemTree |> Maybe.andThen (\item -> getById item.id itemTree)


toArray : ItemTree -> Array Item
toArray itemTree =
    toList itemTree |> Array.fromList


getAncestorIds : String -> ItemTree -> Maybe (List String)
getAncestorIds id itemTree =
    getById id itemTree
        |> Maybe.map (\_ -> getAncestorIdsHelp [] id itemTree)


getAncestorIdsHelp : List String -> String -> ItemTree -> List String
getAncestorIdsHelp ancestorIds id itemTree =
    let
        maybeParent : Maybe Item
        maybeParent =
            getParentById id itemTree
    in
    case maybeParent of
        Just parent ->
            getAncestorIdsHelp (id :: ancestorIds) parent.id itemTree

        Nothing ->
            id :: ancestorIds
