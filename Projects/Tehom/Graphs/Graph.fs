namespace Graph

open Prime

(*
Taken from FSharp.FGL, with the hope of altering it to the point of negligible code borrowing

Copyright 2020 CSBiology

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
 Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

/// Currently using default FSharp map, but it's important to consider using one of Prime's fancy new maps in the future.
type GMap<'k, 'v when 'k : comparison> = FSharp.Collections.Map<'k, 'v>
module GMap = FSharp.Collections.Map

// added
type Vertex = int

///Labeled vertex
type LVertex<'Vertex,'Label> =
    'Vertex * 'Label

///Unlabeled edge
type Edge<'Vertex> =
    'Vertex * 'Vertex

///Labeled edge
type LEdge<'Vertex,'Edge> =
    'Vertex * 'Vertex * 'Edge

///Tuple list of adjacent vertices and the linking edges
type Adj<'Vertex,'Edge> when 'Vertex: comparison =
    List<'Vertex*'Edge>

///Context of a vertice as defined by Martin Erwig. Adjacency of type 'Adj'
type Context<'Vertex,'Label,'Edge> when 'Vertex: comparison=
    Adj<'Vertex,'Edge>*'Vertex*'Label*Adj<'Vertex,'Edge>

///Map of adjacent vertices as key and the linking edges as values
type MAdj<'Vertex,'Edge> when 'Vertex: comparison =
    GMap<'Vertex,'Edge>

///Context of a vertices as defined by Martin Erwig. Adjacency of type 'MAdj'
type MContext<'Vertex,'Label,'Edge> when 'Vertex: comparison =
    MAdj<'Vertex,'Edge> * 'Label * MAdj<'Vertex,'Edge>

///Map of Vertices as keys and MContexts as values
type Graph<'Vertex,'Label,'Edge> when 'Vertex: comparison =
    GMap<'Vertex, MContext<'Vertex,'Label,'Edge>>

// Consider this definition, finding context is definitely harder and requires 2 edge passes, but is probably less intensive on memory.
type AltGraph<'Vertex,'Label,'Edge> when 'Vertex: comparison = {
    Vertices : GMap<'Vertex,'Label>
    Edges : GMap<'Vertex * 'Vertex, 'Edge>
}

///General functions for both directed and undirected graphs
module Graph =

    (* Transition functions *)

    let internal fromAdj<'Vertex,'Edge when 'Vertex: comparison> : Adj<'Vertex,'Edge> -> MAdj<'Vertex,'Edge> =
        GMap.ofList

    let internal fromContext<'Vertex,'Label,'Edge when 'Vertex: comparison> : Context<'Vertex,'Label,'Edge> -> MContext<'Vertex,'Label,'Edge> =
        fun (p, _, l, s) -> fromAdj p, l, fromAdj s

    let internal toAdj<'Vertex,'Edge when 'Vertex: comparison> : MAdj<'Vertex,'Edge> -> Adj<'Vertex,'Edge> =
        GMap.toList

    let internal toContext (v:'Vertex) (mc : MContext<'Vertex,'Label,'Edge>) : Context<'Vertex,'Label,'Edge> =
        mc
        |> fun (p, l, s) -> toAdj p, v, l, toAdj s


    (* Compose Graphs *)

    let internal composeGraph c p v s (g:Graph<'Vertex,'Label,'Edge>) =
        g
        // add vertice as succeeding to previous vertices
        |> List.foldBack (fun (value, edge) g ->
            let p, l, s = Map.find value g
            let s = GMap.add value edge s
            Map.add value (p, l, s) g
        ) p
        // add vertice as previous to succeeding vertices
        |> List.foldBack (fun (edge, value) g ->
            let p, l, s = Map.find value g
            let p = GMap.add value edge p
            Map.add value (p, l, s) g
        ) s
        // add vertice
        |> Map.add v (fromContext c)

    let internal compose c g =
        let p, v, _, s = c
        composeGraph c p v s g

    let internal decomposeGraph v p s g : Graph<'Vertex,'Label,'Edge>=
        // collect all ajacent vertices
        p @ s
        |> List.distinct
        |> List.map fst
        // remove vertice from adjacent vertices contexts
        |> List.fold (fun g value ->
            let p, l, s = Map.find value g
            let p = GMap.remove v p
            let s = GMap.remove v s
            Map.add value (p, l, s) g
        ) g
        // remove vertice
        |> GMap.remove v

    /// Lookup a context in the graph. If the binding exists, it returns the context and the graph minus the vertex and its edges. Raising KeyNotFoundException if no binding exists in the graph.
    let decompose (v:'Vertex) (g: Graph<'Vertex,'Label,'Edge>) = 
        GMap.find v g
        |> fun mc ->
            let p, v, _, s as c = toContext v mc
            let g = decomposeGraph v p s g
            c, g

    /// Lookup a context in the graph. If the binding exists, it returns a Some value of the context and the graph minus the vertex and its edges. If it doesn't exist, returns None and the initial graph.
    let tryDecompose (v:'Vertex) (g: Graph<'Vertex,'Label,'Edge>) = 
        match GMap.tryFind v g with
        | Some mc ->
            let p, v, _, s as c = toContext v mc
            let g = decomposeGraph v p s g
            Some c, g
        | _ ->
            None, g

    /// If the given graph contains at least one vertex, returns a Some value of the first context and the graph minus the associated vertex and its edges. If the graph is empty, returns None and the initial graph.
    let decomposeFirst g : Context<'Vertex,'Label,'Edge> option * Graph<'Vertex,'Label,'Edge> =
        match GMap.tryFindKey (fun _ _ -> true) g with
        | Some v -> tryDecompose v g
        | _ -> None, g


    (* General *)

    /// Returns true, if the Graph does not contain any vertices. Returns false, if not.
    let isEmpty<'Vertex,'Label,'Edge when 'Vertex: comparison> : Graph<'Vertex,'Label,'Edge> -> bool =
        GMap.isEmpty

    /// Creates a new, empty graph.
    let empty : Graph<'Vertex,'Label,'Edge> =
        GMap.empty
    
    /// Lookup a context in the graph, returning a Some value if a binding exists and None if not.
    let tryGetContext v (g:Graph<'Vertex,'Label,'Edge>) : Context<'Vertex,'Label,'Edge> option =
        GMap.tryFind v g
        |> Option.map (toContext v)

    /// Lookup a context in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    let getContext v (g:Graph<'Vertex,'Label,'Edge>) : Context<'Vertex,'Label,'Edge> =
        GMap.find v g
        |> toContext v

    (* Iterative *)

    /// Maps contexts of the graph.
    let mapContexts (mapping : Context<'Vertex,'Label,'Edge> -> 'T) (g: Graph<'Vertex,'Label,'Edge>) : GMap<'Vertex,'T>=
        g
        |> GMap.map (fun v mc ->  mapping (toContext v mc))
    
    /// Folds over the contexts in the graph.
    let foldContexts (state: 'State) (folder : 'State -> Context<'Vertex,'Label,'Edge> -> 'State) (g: Graph<'Vertex,'Label,'Edge>) : 'State =
        g
        |> GMap.fold (fun s v mc -> folder s (toContext v mc)) state

    /// Performs a given function on every edge of the graph.
    let iterContexts (action : Context<'Vertex,'Label,'Edge> -> unit) (g: Graph<'Vertex,'Label,'Edge>) : unit = 
        g
        |> GMap.iter (fun v mc ->  action (toContext v mc))

    /// Join two graphs, merging edges and taking labels from the second graph.
    let join (a : Graph<'Vertex,'Label,'Edge>) (b : Graph<'Vertex,'Label,'Edge>) =
        foldContexts a (fun (state : Graph<'Vertex,'Label,'Edge>) (left, vertex, label, right) ->
            let context =
                match tryGetContext vertex state with
                | Some (left', _, label', right') ->
                    left @ left', vertex, label', right @ right'
                | None ->
                    left, vertex, label, right

            GMap.add vertex (fromContext context) state
        ) b

/// Functions for vertices of both directed and undirected graphs
module Vertices = 

    /// Adds a labeled vertex to the graph.
    let add ((v, l): LVertex<'Vertex,'Label>) (g:Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'Label,'Edge> =
        GMap.add v (GMap.empty, l, GMap.empty) g

    /// Adds a list of labeled vertices to the graph.
    let addMany (vertices:list<LVertex<'Vertex,'Label>>) (g:Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'Label,'Edge> =
        List.fold (fun g vertex -> add vertex g) g vertices

    /// Removes a vertex from the graph.
    let remove (v:'Vertex) (g:Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'Label,'Edge> =
        Graph.decompose v g
        |> snd

    /// Removes a list of vertices from the graph.
    let removeMany nList (g:Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'Label,'Edge> =
        List.fold (fun g' v -> remove v g') g nList

    /// Evaluates the number of vertices in the graph.
    let count (g: Graph<'Vertex,'Label,'Edge>) : int = 
        GMap.count g

    /// Returns true, if the vertex v is contained in the graph. Otherwise, it returns false.
    let contains v (g: Graph<'Vertex,'Label,'Edge>) : bool =
        GMap.containsKey v g

    /// Lookup a labeled vertex in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    let find (v: 'Vertex) (g: Graph<'Vertex,'Label,'Edge>) : LVertex<'Vertex,'Label> = 
        GMap.find v g
        |> fun (_, l, _) -> v, l

    /// Lookup a labeled vertex in the graph, returning a Some value if a binding exists and None if not.
    let tryFind (v: 'Vertex) (g: Graph<'Vertex,'Label,'Edge>) : LVertex<'Vertex,'Label> option = 
        GMap.tryFind v g
        |> Option.map (fun (_, l, _) -> v, l)    

    /// Creates a list of all vertices and their labels.
    let toList (g: Graph<'Vertex,'Label,'Edge>) : LVertex<'Vertex,'Label> list=
        GMap.toList g
        |> List.map (fun (v, (_, l, _)) -> v, l)

    /// Maps the vertexlabels of the graph.
    let map (mapping: 'Vertex -> 'Label -> 'RLabel) (g: Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'RLabel,'Edge>=
        g
        |> GMap.map (fun vertex (p, l, s) ->
            p, mapping vertex l, s)

    /// Maps the vertexlabels of the graph. The mapping function also receives an ascending integer index.
    let mapi (mapping: int -> 'Vertex -> 'Label -> 'RLabel) (g: Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'RLabel,'Edge> =
        g
        |> GMap.toArray
        |> Array.mapi (fun i (v,c) -> v,(i,c))
        |> GMap.ofArray
        |> GMap.map (fun vertex (i,(p, l, s)) ->
            p, mapping i vertex l, s)
    
    /// Performs a given function on every vertex and its label of a graph.
    let iter (action: 'Vertex -> 'Label -> unit) (g: Graph<'Vertex,'Label,'Edge>) : unit=
        g
        |> Graph.iterContexts (fun (_, v, l, _) -> action v l)

    /// Performs a given function on every vertex and its label of a graph. The mapping function also receives an ascending integer index.
    let iteri (action: int -> 'Vertex -> 'Label -> unit) (g: Graph<'Vertex,'Label,'Edge>) : unit =
        let mutable i = 0
        g
        |> GMap.iter (fun vertex (_, l, _) ->
            action i vertex l
            i <- i + 1)
 
    /// Applies a function folder to each vertex of the graph, threading an accumulator argument through the computation.
    let fold (state: 'T) (folder: 'T -> 'Vertex -> 'Label -> 'T) (g: Graph<'Vertex,'Label,'Edge>) : 'T = 
        g
        |> Graph.foldContexts state (fun acc (_, v, l, _) -> folder acc v l)

    /// Returns a new graph containing only the vertices for which the given predicate returns true.
    let filter (predicate : 'Vertex -> 'Label -> bool) (g:Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'Label,'Edge> =
        let verticesToRemove = 
            g
            |> toList
            |> List.choose (fun (v,l) -> if predicate v l then None else Some v)
        removeMany verticesToRemove g

    /// Returns a new graph containing only the vertices for which the given predicate returns true.
    /// TODO: optimize
    let choose (predicate : 'Vertex -> 'Label -> 'RLabel option) (g:Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'RLabel,'Edge> =
        let verticesToRemove =
            g
            |> toList
            |> List.choose (fun (v,l) -> if Option.isSome (predicate v l) then None else Some v)
        removeMany verticesToRemove g
        |> map (fun v l -> Option.get (predicate v l))

/// Functions for edges of both directed and undirected graphs
module Edges =

    ///Map edges of the graph.
    let map
        (mapper: 'Vertex -> 'Vertex -> 'Edge -> 'REdge)
        (g: Graph<'Vertex,'Label,'Edge>)
        : Graph<'Vertex,'Label,'REdge> =
        g
        |> GMap.map (fun vertex (p, l, s) ->
            GMap.map (fun pvertex edge -> mapper pvertex vertex edge) p,
            l,
            GMap.map (fun svertex edge -> mapper vertex svertex edge) s
        )

    ///Filter edges of the graph.
    let filter
        (filter: 'Vertex -> 'Vertex -> 'Edge -> bool)
        (g: Graph<'Vertex,'Label,'Edge>)
        : Graph<'Vertex,'Label,'Edge> =
        g
        |> GMap.map (fun vertex (p, l, s) ->
            GMap.filter (fun pvertex edge -> filter pvertex vertex edge) p,
            l,
            GMap.filter (fun svertex edge -> filter vertex svertex edge) s
        )

    ///Filter and map edges of the graph.
    /// TODO: optimize
    let choose
        (chooser: 'Vertex -> 'Vertex -> 'Edge -> 'REdge option)
        (g: Graph<'Vertex,'Label,'Edge>)
        : Graph<'Vertex,'Label,'REdge> =
        g
        |> GMap.map (fun vertex (p, l, s) ->
            p
            |> GMap.toSeq
            |> Seq.choose (fun (pvertex, edge) ->
                match chooser pvertex vertex edge with
                | Some edge' -> Some (pvertex, edge')
                | None -> None
            )
            |> GMap.ofSeq,
            l,
            s
            |> GMap.toSeq
            |> Seq.choose (fun (svertex, edge) ->
                match chooser vertex svertex edge with
                | Some edge' -> Some (svertex, edge')
                | None -> None
            )
            |> GMap.ofSeq
        )

    ///Transform a directed graph to an undirected graph.
    let undirect
        (join: 'Edge -> 'Edge -> 'Edge)
        (g: Graph<'Vertex,'Label,'Edge>)
        : Graph<'Vertex,'Label,'Edge> =

        g
        |> GMap.map (fun _ (p,l,s) ->
            GMap.empty,
            l,
            GMap.fold (fun s' v' e ->
                let e' =
                    match GMap.tryFind v' s' with
                    | Some e' -> join e e'
                    | None -> e
                GMap.add v' e' s'
            ) s p
        )

    ///Undirect some edges of a graph.
    let undirect2
        (predicate: 'Vertex -> 'Vertex -> 'Edge -> bool)
        (join: 'Edge -> 'Edge -> 'Edge)
        (g: Graph<'Vertex,'Label,'Edge>)
        : Graph<'Vertex,'Label,'Edge> =
        g
        |> GMap.map (fun vertex (p, l, s) ->
            let p, p' = GMap.partition (fun pvertex edge -> predicate vertex pvertex edge) p
            p',
            l,
            GMap.fold (fun s' v' e ->
                let e' =
                    match GMap.tryFind v' s' with
                    | Some e' -> join e e'
                    | None -> e
                GMap.add v' e' s'
            ) s p
        )