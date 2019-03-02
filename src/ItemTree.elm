module ItemTree exposing (ItemForest, ItemTree, itemToTree, toForest)

import Array exposing (Array)
import ItemLookup exposing (Item, ItemLookup, getChildrenById, getRootItems)


type alias ItemForest =
    Array ItemTree


type ItemTree
    = Tree Item ItemForest


toForest : ItemLookup -> ItemForest
toForest itemTree =
    getRootItems itemTree |> List.map (itemToTree itemTree) |> Array.fromList


itemToTree : ItemLookup -> Item -> ItemTree
itemToTree itemTree item =
    Tree item (getChildrenById item.id itemTree |> List.map (itemToTree itemTree) |> Array.fromList)



--flattenForest : Forest -> List Item
--flattenForest forest =
--    forest |> Array.toList|> List.concatMap (\(Tree item innerForest) -> item :: flattenForest innerForest)
--        |> Array.
