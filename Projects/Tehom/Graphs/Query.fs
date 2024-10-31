namespace FGL

open Prime
open System
open Aether
open FGL

module Query =

    type Path<'Vertex> = 'Vertex list

    type LPath<'Vertex, 'Label> = LVertex<'Vertex, 'Label> list

    type RTree<'Vertex> = Path<'Vertex> list

    type LRTree<'Vertex, 'Label> = LPath<'Vertex, 'Label> list

    /// Tree of shortest paths from a certain node to the rest of the (reachable) nodes.
    /// Corresponds to 'dijkstra' applied to a heap in which the only known node is the starting node, with a path of length
    /// 0 leading to it. The edge labels of type @b@ are the edge weights; negative edge weights are not supported.
    let spTree (v : 'Vertex) (g : Graph<'Vertex, 'Label, 'Distance>) : LRTree<'Vertex, 'Distance> =

        // heap is the main orderer of paths
        let decomposeHeap heap =
            let firstValue = heap |> OSet.toSeq |> Seq.head
            let heap' = OSet.remove firstValue heap
            firstValue, heap'

        let composeHeap composer newVertices heap =
            newVertices
            |> List.map composer
            |> OSet.concat heap

        /// Dijkstra's shortest path algorithm.
        /// The edge labels of type @b@ are the edge weights; negative edge weights are not supported.
        /// Basically a graph fold with extra rules. Tail recursive unlike FGL implementation.
        let rec dijkstra
            (minPathTree : LRTree<'Vertex, 'Distance>)
            (heap : OSet<LPath<'Vertex,'Distance>>)
            (graph : Graph<'Vertex, 'Label, 'Distance>)
            : LRTree<'Vertex, 'Distance> =

            if OSet.isEmpty heap || Graph.isEmpty graph then
                minPathTree
            else
                let minPath, heap' = decomposeHeap heap

                match minPath with
                | [] -> minPathTree
                | (minVertex, minVertexDistance) :: _ ->

                    let context, graph' = Graph.tryDecompose minVertex graph

                    let minPathTree', heap' =
                        match context with
                        | Some (_, _, _, rightEdge) ->

                            // rebuild heap and add minpath
                            let minPathTree' =
                                minPath :: minPathTree

                            let heap' =
                                composeHeap (fun (v, l) ->
                                    [v, l + minVertexDistance] @ minPath
                                ) rightEdge heap'

                            minPathTree', heap'

                        | None ->

                            // check next min value on heap
                            minPathTree, heap'

                    dijkstra minPathTree' heap' graph'

        dijkstra List.empty (OSet.ofSeq1 [[v, 0u]]) g

    /// Find the first path in a tree that starts with the given node.
    /// Returns an empty list if there is no such path.
    [<TailCall>]
    let rec findP (v : 'Vertex) (p : LRTree<'Vertex, 'Distance>) : LVertex<'Vertex, 'Distance> list =
        match p with
        | [] -> []
        | []::ps -> findP v ps
        | ((w,_)::_)::ps when not (v = w) -> findP v ps
        | p::_ -> p

    /// Return the distance to the given node in the given tree.
    /// Returns 'Nothing' if the given node is not reachable.
    let getDistance (v : 'Vertex) (t : LRTree<'Vertex, 'Distance>) : 'Distance option =
        match findP v t with
        | [] -> None
        | (_,d)::_ -> Some d

    /// Length of the shortest path between two nodes, if any.
    /// Returns 'Nothing' if there is no path, and @'Just' <path length>@ otherwise.
    /// The edge labels of type @b@ are the edge weights; negative edge weights are not supported.
    let spLength (v1 : 'Vertex) (v2 : 'Vertex) (g : Graph<'Vertex, 'Label, 'Distance>) : 'Distance option =
        spTree v1 g |> getDistance v2

    let getLPath (v : 'Vertex) : LRTree<'Vertex, 'Distance> -> LPath<'Vertex, 'Distance> =
        findP v
        >> List.rev

    let getLPathNodes (v : 'Vertex) : LRTree<'Vertex, 'Distance> -> Path<'Vertex> =
        getLPath v
        >> List.map fst

    let sp (v1 : 'Vertex) (v2 : 'Vertex) (g : Graph<'Vertex, 'Label, 'Distance>) : Path<'Vertex> =
        spTree v1 g
        |> getLPathNodes v2

    let sp2 (v1 : 'Vertex) (v2 : 'Vertex) (g : Graph<'Vertex, 'Label, 'Distance>) : LPath<'Vertex, 'Distance> =
        spTree v1 g
        |> getLPath v2