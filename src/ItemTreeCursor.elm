module ItemTreeCursor exposing (ItemTreeCursor, forId, getItemLookup, nest, unNest)

import Array exposing (Array)
import ItemLookup exposing (ItemLookup)
import ItemTree exposing (ItemForest, ItemTree)


type alias ItemTreeCursor =
    { itemLookup : ItemLookup, itemForest : ItemForest, path : ( List Int, Int ) }


forId : String -> ItemLookup -> Maybe ItemTreeCursor
forId id itemLookup =
    let
        itemForest =
            ItemTree.toForest itemLookup
    in
    ItemLookup.getAncestorIds id itemLookup
        |> Maybe.map
            (\ancestorIds ->
                let
                    _ =
                        1
                in
                { itemLookup = itemLookup, itemForest = itemForest, path = ( [], 0 ) }
            )


ancestorIdsToIndices : List Int -> List String -> ItemForest -> Maybe (List Int)
ancestorIdsToIndices ancestorIndices ancestorIds itemForest =
    case ancestorIds of
        [] ->
            Just ancestorIndices

        id :: rest ->
            ItemTree.getIdxForestTupleForItemId id itemForest
                |> Maybe.andThen
                    (\( idx, forest ) ->
                        let
                            newAncestorIndices =
                                ancestorIndices ++ [ idx ]
                        in
                        ancestorIdsToIndices newAncestorIndices rest forest
                    )


nest : ItemTreeCursor -> Maybe ItemTreeCursor
nest cursor =
    Just cursor


unNest : ItemTreeCursor -> Maybe ItemTreeCursor
unNest cursor =
    Just cursor


getItemLookup : ItemTreeCursor -> ItemLookup
getItemLookup cursor =
    cursor.itemLookup
