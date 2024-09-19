namespace FGL

open System
open Aether
open FGL

module Query =

(*

-- (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]

-- | Shortest path algorithms
module Data.Graph.Inductive.Query.SP(
      spTree
    , sp
    , spLength
    , dijkstra
    , LRTree
    , H.Heap
) where

import qualified Data.Graph.Inductive.Internal.Heap as H

import Data.Graph.Inductive.Graph



import Data.Graph.Inductive.Internal.RootPath


-- (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]
-- | Inward directed trees as lists of paths.
module Data.Graph.Inductive.Internal.RootPath (
    -- * Types
    RTree,LRTree,
    -- * Operations
    getPath,getLPath,
    getDistance,
    getLPathNodes
) where

import Data.Graph.Inductive.Graph

type LRTree a = [LPath a]
type RTree = [Path]

first :: ([a] -> Bool) -> [[a]] -> [a]
first p xss  = case filter p xss of
                 []   -> []
                 x:_  -> x

-- | Find the first path in a tree that starts with the given node.
--
--   Returns an empty list if there is no such path.
findP :: Node -> LRTree a -> [LNode a]
findP _ []                                = []
findP v (LP []:ps)                        = findP v ps
findP v (LP (p@((w,_):_)):ps) | v==w      = p
                              | otherwise = findP v ps

getPath :: Node -> RTree -> Path
getPath v = reverse . first ((==v) . head)

getLPath :: Node -> LRTree a -> LPath a
getLPath v = LP . reverse . findP v

-- | Return the distance to the given node in the given tree.
--
--   Returns 'Nothing' if the given node is not reachable.
getDistance :: Node -> LRTree a -> Maybe a
getDistance v t = case findP v t of
  []      -> Nothing
  (_,d):_ -> Just d

getLPathNodes :: Node -> LRTree a -> Path
getLPathNodes v = (\(LP p)->map fst p) . getLPath v


*)

    type Path<'Vertex> = 'Vertex list

    type LPath<'Vertex, 'Label> = LP of LVertex<'Vertex, 'Label> list

    type RTree<'Vertex> = Path<'Vertex> list

    type LRTree<'Vertex, 'Label> = LPath<'Vertex, 'Label> list

    let expand (d : 'Distance) (LP p) ((_,_,_,s) : Context<'Vertex, 'Label, 'Distance>) : (Map<'Distance, LPath<'Vertex, 'Distance>> list) =
        s
        // switched v and l since its edges that are supposed to have length, no?
        |> List.map (fun (v, l) ->
            Map.empty
            |> Map.add (l + d) (LP ([v, l + d] @ p))
        )


(*
-- | Dijkstra's shortest path algorithm.
--
--   The edge labels of type @b@ are the edge weights; negative edge
--   weights are not supported.
*)

    [<TailCall>]
    let rec dijkstra (h : Map<'Distance, LPath<'Vertex,'Distance>>) (g : Graph<'Vertex, 'Label, 'Distance>) : LRTree<'Vertex, 'Distance> =

        if not (Map.isEmpty h) && not (Graph.isEmpty g) then

            let min = h |> Map.keys |> Seq.head
            let minValue = Map.find min h
            let h' = Map.remove min h

            match minValue with
            | LP ((v, d) :: _) as p ->
                match Graph.tryDecompose v g with
                | Some context, g' ->
                    let expand = expand d p context

                    let mergeMaps maps =
                        maps
                        |> List.map Map.toList
                        |> List.concat
                        |> Map.ofList

                    [p] @ (dijkstra (mergeMaps ([h'] @ expand)) g')
                | None, g' ->
                    dijkstra h' g'

            | _ ->
                []
        else
            []

(*
-- | Tree of shortest paths from a certain node to the rest of the
--   (reachable) nodes.
--
--   Corresponds to 'dijkstra' applied to a heap in which the only known node is
--   the starting node, with a path of length 0 leading to it.
--
--   The edge labels of type @b@ are the edge weights; negative edge
--   weights are not supported.
*)
    let spTree (v : 'Vertex) : Graph<'Vertex, 'Label, 'Distance> -> LRTree<'Vertex, 'Distance> = dijkstra (
        Map.empty
        |> Map.add 0u (LP [v, 0u])
    )


(*
-- | Find the first path in a tree that starts with the given node.
--
--   Returns an empty list if there is no such path.
findP :: Node -> LRTree a -> [LNode a]
findP _ []                                = []
findP v (LP []:ps)                        = findP v ps
findP v (LP (p@((w,_):_)):ps) | v==w      = p
                              | otherwise = findP v ps
*)

    let rec findP (v : 'Vertex) (p : LRTree<'Vertex, 'Distance>) : LVertex<'Vertex, 'Distance> list =
        match p with
        | [] -> []
        | LP []::ps -> findP v ps
        | LP ((w,_)::_)::ps when not (v = w) -> findP v ps
        | LP p::_ -> p


(*
-- | Return the distance to the given node in the given tree.
--
--   Returns 'Nothing' if the given node is not reachable.

getDistance :: Node -> LRTree a -> Maybe a
getDistance v t = case findP v t of
  []      -> Nothing
  (_,d):_ -> Just d
*)

    let getDistance (v : 'Vertex) (t : LRTree<'Vertex, 'Distance>) : 'Distance option =
        match findP v t with
        | [] -> None
        | ((_,d)::_) -> Some d

    /// Length of the shortest path between two nodes, if any.
    /// Returns 'Nothing' if there is no path, and @'Just' <path length>@ otherwise.
    /// The edge labels of type @b@ are the edge weights; negative edge weights are not supported.

    let spLength (v1 : 'Vertex) (v2 : 'Vertex) (g : Graph<'Vertex, 'Label, 'Distance>) : 'Distance option =
        spTree v1 g |> getDistance v2


    let getLPath (v : 'Vertex) : LRTree<'Vertex, 'Distance> -> LPath<'Vertex, 'Distance> =
        findP v
        >> List.rev
        >> LP

    let getLPathNodes (v : 'Vertex) : LRTree<'Vertex, 'Distance> -> Path<'Vertex> =
        getLPath v
        >> fun (LP x) -> List.map fst x

    let sp (v1 : 'Vertex) (v2 : 'Vertex) (g : Graph<'Vertex, 'Label, 'Distance>) : Path<'Vertex> =
        spTree v1 g
        |> getLPathNodes v2




(*

-- | Shortest path between two nodes, if any.
--
--   Returns 'Nothing' if the destination is not reachable from the
--   start node, and @'Just' <path>@ otherwise.
--
--   The edge labels of type @b@ are the edge weights; negative edge
--   weights are not supported.
sp :: (Graph gr, Real b)
    => Node -- ^ Start
    -> Node -- ^ Destination
    -> gr a b
    -> Maybe Path
sp s t g = case getLPathNodes t (spTree s g) of
  [] -> Nothing
  p  -> Just p




*)