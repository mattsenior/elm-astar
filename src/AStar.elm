module AStar exposing (aStar)

import Set exposing (Set)
import Dict exposing (Dict)
import PairingHeap exposing (PairingHeap)
import List.Extra


type alias AStar comparable number =
    { visited : Set comparable
    , waiting : PairingHeap comparable number
    , score : Dict comparable number
    , memoHeur : Dict comparable number
    , cameFrom : Dict comparable comparable
    , end : Maybe comparable
    }


aStarInit : comparable -> AStar comparable number
aStarInit start =
    { visited = Set.empty
    , waiting = PairingHeap.insert ( 0, start ) PairingHeap.empty
    , score = Dict.singleton start 0
    , memoHeur = Dict.empty
    , cameFrom = Dict.empty
    , end = Nothing
    }


runAStar :
    (comparable -> Set comparable)
    -> (comparable -> comparable -> number)
    -> (comparable -> number)
    -> (comparable -> Bool)
    -> comparable
    -> AStar comparable number
runAStar graph dist heur goal start =
    let
        go s =
            case PairingHeap.findMin s.waiting of
                Nothing ->
                    s

                Just ( _, x ) ->
                    if goal x then
                        { s | end = Just x }
                    else
                        go <|
                            List.foldl
                                (expand x)
                                { s
                                    | waiting = PairingHeap.deleteMin s.waiting
                                    , visited = Set.insert x s.visited
                                }
                                (Set.diff (graph x) s.visited |> Set.toList)

        expand : comparable -> comparable -> AStar comparable number -> AStar comparable number
        expand x y s =
            let
                v =
                    Dict.get x s.score
                        |> Maybe.withDefault 0
                        |> (+) (dist x y)
            in
                -- Should probably switch this for a queue with an efficient membership test
                if PairingHeap.toSortedList s.waiting |> List.map Tuple.second |> List.member y then
                    if v < (Dict.get y s.score |> Maybe.withDefault 0) then
                        link x y v s
                    else
                        s
                else
                    link x y v { s | memoHeur = Dict.insert y (heur y) s.memoHeur }

        link x y v s =
            let
                waiting =
                    PairingHeap.insert ( v + (Dict.get y s.memoHeur |> Maybe.withDefault 0), y ) s.waiting
            in
                { s
                    | cameFrom = Dict.insert y x s.cameFrom
                    , score = Dict.insert y v s.score
                    , waiting = waiting
                }
    in
        go (aStarInit start)


aStar :
    (comparable -> Set comparable)
    -> (comparable -> comparable -> number)
    -> (comparable -> number)
    -> (comparable -> Bool)
    -> comparable
    -> Maybe (List comparable)
aStar graph dist heur goal start =
    let
        s =
            runAStar graph dist heur goal start
    in
        s.end
            |> Maybe.map
                (List.reverse
                    << List.Extra.iterate
                        (\y ->
                            Dict.get y s.cameFrom
                                |> Maybe.andThen
                                    (\x ->
                                        if x == start then
                                            Nothing
                                        else
                                            Just x
                                    )
                        )
                )
