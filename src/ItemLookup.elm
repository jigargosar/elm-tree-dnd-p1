module ItemLookup exposing (Item, ItemLookup, fromList, getAncestorIds, getById, getChildrenOfId, getRootItems, insertAll, toArray, toList)

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra


type alias Item =
    { id : String
    , rev : Maybe String
    , title : String
    , pid : Maybe String
    , childIds : List String
    , rootIdx : Int
    }


type alias ItemLookup =
    Dict String Item


fromList : List Item -> ItemLookup
fromList itemList =
    itemList
        |> List.map (\item -> ( item.id, item ))
        |> Dict.fromList


insertAll : List Item -> ItemLookup -> ItemLookup
insertAll items itemLookup =
    List.foldl (\item -> Dict.insert item.id item) itemLookup items


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
    toList itemLookup
        |> List.filter
            (\item ->
                item.pid
                    |> Maybe.map (\_ -> False)
                    |> Maybe.withDefault True
            )
        |> List.sortBy (\item -> item.rootIdx)


getChildrenOfId : String -> ItemLookup -> Maybe (List Item)
getChildrenOfId parentId itemLookup =
    getById parentId itemLookup |> Maybe.map (\parent -> List.filterMap (\cid -> getById cid itemLookup) parent.childIds)


getSiblingsOfId : String -> ItemLookup -> Maybe (List Item)
getSiblingsOfId id itemLookup =
    getParentById id itemLookup
        |> Maybe.andThen (\parent -> getChildrenOfId parent.id itemLookup)


getPrevSibling : String -> ItemLookup -> Maybe Item
getPrevSibling id itemLookup =
    getParentById id itemLookup
        |> Maybe.andThen
            (\parent ->
                let
                    maybePrevSibling : Maybe Item
                    maybePrevSibling =
                        parent.childIds
                            |> List.reverse
                            |> List.Extra.dropWhile (\cid -> cid /= id)
                            |> List.Extra.dropWhile (\cid -> cid == id)
                            |> List.head
                            |> Maybe.andThen (\cid -> getById cid itemLookup)
                in
                maybePrevSibling
            )



--
--        |> Array.
