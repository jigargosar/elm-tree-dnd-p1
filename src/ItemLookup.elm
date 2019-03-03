module ItemLookup exposing (Item, ItemLookup, fromList, getAncestorIds, getById, getChildrenOfId, getRootItems, toArray, toList)

import Array exposing (Array)
import Dict exposing (Dict)


type alias Item =
    { id : String
    , title : String
    , pid : Maybe String
    }


type alias ItemLookup =
    Dict String Item


fromList : List Item -> ItemLookup
fromList itemList =
    itemList
        |> List.map (\item -> ( item.id, item ))
        |> Dict.fromList


toList : ItemLookup -> List Item
toList itemLookup =
    itemLookup |> Dict.values


getById : String -> ItemLookup -> Maybe Item
getById id itemLookup =
    Dict.get id itemLookup


getParentById : String -> ItemLookup -> Maybe Item
getParentById id itemLookup =
    getById id itemLookup |> Maybe.andThen (\item -> getById item.id itemLookup)


toArray : ItemLookup -> Array Item
toArray itemLookup =
    toList itemLookup |> Array.fromList


getAncestorIds : String -> ItemLookup -> Maybe (List String)
getAncestorIds id itemLookup =
    getById id itemLookup
        |> Maybe.map (\_ -> getAncestorIdsHelp [] id itemLookup)


getAncestorIdsHelp : List String -> String -> ItemLookup -> List String
getAncestorIdsHelp ancestorIds id itemLookup =
    let
        maybeParent : Maybe Item
        maybeParent =
            getParentById id itemLookup

        newAncestorIds : List String
        newAncestorIds =
            id :: ancestorIds
    in
    case maybeParent of
        Just parent ->
            getAncestorIdsHelp newAncestorIds parent.id itemLookup

        Nothing ->
            newAncestorIds


getRootItems : ItemLookup -> List Item
getRootItems itemLookup =
    toList itemLookup |> List.filterMap (\item -> item.pid |> Maybe.map (\_ -> item))


getChildrenOfId : String -> ItemLookup -> List Item
getChildrenOfId parentId itemLookup =
    toList itemLookup |> List.filter (\parent -> parentId == parent.id)


getSiblingsOfId : String -> ItemLookup -> Maybe (List Item)
getSiblingsOfId id itemLookup =
    getParentById id itemLookup
        |> Maybe.map (\parent -> getChildrenOfId parent.id itemLookup)


getPrevSibling : String -> ItemLookup -> Maybe (List Item)
getPrevSibling id itemLookup =
    getSiblingsOfId id itemLookup



--
--        |> Array.
