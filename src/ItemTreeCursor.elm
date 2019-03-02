module ItemTreeCursor exposing (ItemTreeCursor)

import Array exposing (Array)
import Dict exposing (Dict)
import ItemTree exposing (ItemTree)


type alias ItemTreeCursor =
    { itemTree : ItemTree, ancestorIds : List String }


forId : String -> ItemTree -> Maybe ItemTreeCursor
forId id itemTree =
    ItemTree.getAncestorIds id itemTree
        |> Maybe.map (\ancestorIds -> { itemTree = itemTree, ancestorIds = ancestorIds })
