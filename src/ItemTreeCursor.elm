module ItemTreeCursor exposing (ItemTreeCursor, forId, getItemLookup, nest, unNest)

import Array exposing (Array)
import ItemLookup exposing (ItemLookup)
import ItemTree exposing (ItemForest, ItemTree)


type alias Path =
    ( Int, List Int )


type alias ItemTreeCursor =
    { itemLookup : ItemLookup, itemForest : ItemForest, path : Path }


forId : String -> ItemLookup -> Maybe ItemTreeCursor
forId id itemLookup =
    let
        itemForest =
            ItemTree.toForest itemLookup
    in
    ItemLookup.getAncestorIds id itemLookup
        |> Maybe.andThen (ancestorIdsToPath itemForest)
        |> Maybe.map
            (\path ->
                { itemLookup = itemLookup, itemForest = itemForest, path = path }
            )


ancestorIdsToPath : ItemForest -> List String -> Maybe Path
ancestorIdsToPath itemForest ancestorIds =
    ancestorIdsToIndices [] ancestorIds itemForest
        |> Maybe.andThen
            (\indices ->
                case indices of
                    [] ->
                        Nothing

                    idx :: restIndices ->
                        Just ( idx, restIndices )
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
                                idx :: ancestorIndices
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
