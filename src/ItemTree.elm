module ItemTree exposing (ItemForest, ItemTree, getIdxForestTupleForItemId, itemToTree, toForest)

import Array exposing (Array)
import ItemLookup exposing (Item, ItemLookup, getChildrenOfId, getRootItems)


type alias ItemForest =
    Array ItemTree


type ItemTree
    = Tree Item ItemForest


toForest : ItemLookup -> ItemForest
toForest itemTree =
    getRootItems itemTree |> List.map (itemToTree itemTree) |> Array.fromList


itemToTree : ItemLookup -> Item -> ItemTree
itemToTree itemTree item =
    Tree item (getChildrenOfId item.id itemTree |> List.map (itemToTree itemTree) |> Array.fromList)


getIdxForestTupleForItemId : String -> ItemForest -> Maybe ( Int, ItemForest )
getIdxForestTupleForItemId id forest =
    Array.toIndexedList forest
        |> List.filter (\( _, Tree item _ ) -> item.id == id)
        |> List.head
        |> Maybe.map (\( idx, Tree _ f ) -> ( idx, f ))



--flattenForest : Forest -> List Item
--flattenForest forest =
--    forest |> Array.toList|> List.concatMap (\(Tree item innerForest) -> item :: flattenForest innerForest)
--        |> Array.
