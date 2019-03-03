module ItemTree exposing (ItemForest, ItemTree, getIdxForestTupleForItemId, toForest)

import Array exposing (Array)
import ItemLookup exposing (Item, ItemLookup, getChildrenOfId, getRootItems)


type alias ItemForest =
    Array ItemTree


type ItemTree
    = Tree Item ItemForest


toForest : ItemLookup -> ItemForest
toForest itemTree =
    getRootItems itemTree |> List.filterMap (itemToTree itemTree) |> Array.fromList


itemToTree : ItemLookup -> Item -> Maybe ItemTree
itemToTree itemLookup item =
    let
        foo : Item -> Maybe (Array ItemTree) -> Maybe (Array ItemTree)
        foo child maybeForest =
            Maybe.andThen
                (\forest -> itemToTree itemLookup child |> Maybe.map (\tree -> Array.push tree forest))
                maybeForest
    in
    getChildrenOfId item.id itemLookup
        |> Maybe.andThen (List.foldl foo (Just Array.empty))
        |> Maybe.map (Tree item)


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
