module ItemTreeCursor exposing (ItemTreeCursor, forId, nest, tree, unNest)

import Array exposing (Array)
import ItemLookup exposing (ItemLookup)


type alias ItemTreeCursor =
    { itemTree : ItemLookup, ancestorIds : Array String }


forId : String -> ItemLookup -> Maybe ItemTreeCursor
forId id itemTree =
    ItemLookup.getAncestorIds id itemTree
        |> Maybe.map (\ancestorIds -> { itemTree = itemTree, ancestorIds = ancestorIds })


nest : ItemTreeCursor -> Maybe ItemTreeCursor
nest cursor =
    Just cursor


unNest : ItemTreeCursor -> Maybe ItemTreeCursor
unNest cursor =
    Just cursor


tree : ItemTreeCursor -> ItemLookup
tree cursor =
    cursor.itemTree
