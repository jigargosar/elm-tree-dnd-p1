module ItemLookup exposing (Item, ItemLookup, fromList, getAncestorIds, getById, getChildrenById, getRootItems, toArray, toList)

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
toList itemTree =
    itemTree |> Dict.values


getById : String -> ItemLookup -> Maybe Item
getById id itemTree =
    Dict.get id itemTree


getParentById : String -> ItemLookup -> Maybe Item
getParentById id itemTree =
    getById id itemTree |> Maybe.andThen (\item -> getById item.id itemTree)


toArray : ItemLookup -> Array Item
toArray itemTree =
    toList itemTree |> Array.fromList


getAncestorIds : String -> ItemLookup -> Maybe (Array String)
getAncestorIds id itemTree =
    getById id itemTree
        |> Maybe.map (\_ -> getAncestorIdsHelp Array.empty id itemTree)


getAncestorIdsHelp : Array String -> String -> ItemLookup -> Array String
getAncestorIdsHelp ancestorIds id itemTree =
    let
        maybeParent : Maybe Item
        maybeParent =
            getParentById id itemTree

        newAncestorIds : Array String
        newAncestorIds =
            ancestorIds |> Array.push id
    in
    case maybeParent of
        Just parent ->
            getAncestorIdsHelp newAncestorIds parent.id itemTree

        Nothing ->
            newAncestorIds


getRootItems : ItemLookup -> List Item
getRootItems itemTree =
    toList itemTree |> List.filterMap (\item -> item.pid |> Maybe.map (\_ -> item))


getChildrenById : String -> ItemLookup -> List Item
getChildrenById id itemTree =
    toList itemTree
        |> List.filterMap
            (\item ->
                item.pid
                    |> Maybe.andThen
                        (\parentId ->
                            if id == parentId then
                                Just item

                            else
                                Nothing
                        )
            )



--        |> Array.
