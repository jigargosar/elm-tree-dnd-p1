module ItemTreeCursor exposing (ItemTreeCursor, forId, nest, tree, unNest)

import Array exposing (Array)
import ItemTree exposing (ItemTree)


type alias ItemTreeCursor =
    { itemTree : ItemTree, ancestorIds : Array String }


forId : String -> ItemTree -> Maybe ItemTreeCursor
forId id itemTree =
    ItemTree.getAncestorIds id itemTree
        |> Maybe.map (\ancestorIds -> { itemTree = itemTree, ancestorIds = ancestorIds })


nest : ItemTreeCursor -> Maybe ItemTreeCursor
nest cursor =
    Just cursor


unNest : ItemTreeCursor -> Maybe ItemTreeCursor
unNest cursor =
    Just cursor


tree : ItemTreeCursor -> ItemTree
tree cursor =
    cursor.itemTree
