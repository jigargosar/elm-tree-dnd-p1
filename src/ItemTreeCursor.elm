module ItemTreeCursor exposing (ItemTreeCursor)

import Array exposing (Array)
import ItemTree exposing (ItemTree)


type alias ItemTreeCursor =
    { itemTree : ItemTree, ancestorIds : Array String }


forId : String -> ItemTree -> Maybe ItemTreeCursor
forId id itemTree =
    ItemTree.getAncestorIds id itemTree
        |> Maybe.map (\ancestorIds -> { itemTree = itemTree, ancestorIds = ancestorIds })
