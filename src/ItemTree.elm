module ItemTree exposing (Item, ItemTree, fromList, getById, toList)

import Dict exposing (Dict)


type alias Item =
    { id : String
    , title : String
    , pid : Maybe String
    }


type alias ItemTree =
    Dict String Item


fromList : List Item -> ItemTree
fromList itemList =
    itemList
        |> List.map (\item -> ( item.id, item ))
        |> Dict.fromList


toList : ItemTree -> List Item
toList itemTree =
    itemTree |> Dict.values


getById : String -> ItemTree -> Maybe Item
getById id itemTree =
    Dict.get id itemTree
