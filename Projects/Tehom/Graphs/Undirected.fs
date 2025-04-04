namespace Graph.Undirected

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

open System
open Graph

///Functions for vertices of undirected Graphs
module Vertices = 

    (* Neighbours *)

    ///Lists the vertices which are connected to the vertex.
    let neighbours (context:Context<'Vertex,'Label,'Edge>) : 'Vertex list =
        context
        |> fun (_, _, _, s) -> List.map fst s

    (* Properties *)

    ///Evaluates the number of edges associated with the vertex.
    let degree (context:Context<'Vertex,'Label,'Edge>) : int =
        context
        |> fun (_, _, _, s) -> List.length s

    ///Evaluates the clustering coefficient of the vertex.
    let clusteringCoefficient (context:Context<'Vertex,'Label,'Edge>) (g: Graph<'Vertex,'Label,'Edge>) : float=
        context
        |> fun c ->     
            if degree c < 2 then 0.
            else        
                let add1IfInList acc x set = 
                    if List.contains x set then acc + 1
                    else acc
                let neighbours = neighbours c
                let neighbourEdges = 
                    List.fold (fun edgeAmount v' -> 
                        (Graph.getContext v' g
                        |> fun (p',_,_,_) ->
                            (p'
                            |> List.fold (fun acc (x,_) -> add1IfInList acc x neighbours) 0))
                        + edgeAmount
                    ) 0 neighbours
                let degree = List.length neighbours
                (float neighbourEdges) / (float (degree * (degree - 1)))


///Functions for edges of undirected Graphs
module Edges =
     
    (* Properties *)

    ///Evaluates the number of edges in the graph.
    let count (g: Graph<'Vertex,'Label,'Edge>) : int = 
        Map.toArray g
        |> Array.fold (fun c (_,(_,_,s)) -> c + ((Map.toList s) |> List.length)) 0
        |> fun x -> x / 2


    (* General *)

    ///Returns true, if the edge from vertex v1 to vertex v2 is contained in the graph. Otherwise, it returns false.
    let contains v1 v2 (g: Graph<'Vertex,'Label,'Edge>) : bool =
        Map.tryFind v1 g
        |> Option.bind (fun (_, _, s) -> Map.tryFind v2 s)
        |> Option.isSome

    ///Lookup a labeled edge in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    let find (v1:'Vertex) (v2:'Vertex) (g: Graph<'Vertex,'Label,'Edge>) : LEdge<'Vertex,'Edge> =
            Map.find v1 g
            |> fun (_, _, s) -> Map.find v2 s
            |> fun e -> (v1,v2,e)

    ///Lookup a labeled edge in the graph, returning a Some value if a binding exists and None if not.
    let tryFind (v1:'Vertex) (v2:'Vertex) (g: Graph<'Vertex,'Label,'Edge>) : LEdge<'Vertex,'Edge> option =
            Map.tryFind v1 g
            |> Option.bind (fun (_, _, s) -> Map.tryFind v2 s)
            |> Option.map (fun e -> (v1,v2,e))
    
    (* Add and remove *)
    
    ///Adds a labeled, undirected edge to the graph.
    let tryAdd ((v1, v2, edge): LEdge<'Vertex,'Edge>) (g: Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'Label,'Edge> option =
        if (Vertices.contains v1 g |> not) || (Vertices.contains v2 g |> not) || contains v1 v2 g then
            None
        else
            let addToSucceeding v1 v2 g =
                let p, l, s = Map.find v1 g
                let s = Map.add v2 edge s
                Map.add v1 (p, l, s) g

            g
            |> addToSucceeding v1 v2
            |> addToSucceeding v2 v1
            |> Some


    ///Adds a labeled, undirected edge to the graph.
    let add ((v1, v2, edge): LEdge<'Vertex,'Edge>) (g: Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'Label,'Edge> =
        if Vertices.contains v1 g |> not then failwithf "Source Vertex %O does not exist" v1 
        if Vertices.contains v2 g |> not then failwithf "Target Vertex %O does not exist" v2
        if contains v1 v2 g then failwithf "An Edge between Source vertex %O Target Vertex %O does already exist; FSharp.FGL does not allow for duplicate edges" v1 v2

        let addToSucceeding v v' g =
            let p, l, s = Map.find v g
            let s = Map.add v' edge s
            Map.add v (p, l, s) g

        g
        |> addToSucceeding v1 v2
        |> addToSucceeding v2 v1
    
    ///Adds a list of labeled, undirected edges to the graph.
    let tryAddMany (edges : list<LEdge<'Vertex,'Edge>>) (g: Graph<'Vertex,'Label,'Edge>) : Graph<'Vertex,'Label,'Edge> option =
        List.fold (fun g e -> Option.bind (tryAdd e) g) (Some g) edges

    ///Adds a list of labeled, undirected edges to the graph.
    let addMany (edges : list<LEdge<'Vertex,'Edge>>) (g: Graph<'Vertex,'Label,'Edge>) =
        List.fold (fun g e -> add e g) g edges
    
    ///Removes an edge from the graph.
    let remove ((v1, v2): Edge<'Vertex>) (g: Graph<'Vertex,'Label,'Edge>) =

        let removeFromSucceeding v v' g =
            let p, l, s = Map.find v g
            let s = Map.remove v' s
            Map.add v (p, l, s) g

        g
        |> removeFromSucceeding v1 v2
        |> removeFromSucceeding v2 v1
                    
    ///Removes a list of edges from the graph.
    let removeMany (edges : list<Edge<'Vertex>>) (g: Graph<'Vertex,'Label,'Edge>) =
        List.fold (fun g e -> remove e g) g edges

    ///Creates a list of all edges and their labels.
    let toList (g:Graph<'Vertex,'Label,'Edge>) : LEdge<'Vertex,'Edge> list =
        let sourceList = 
            g
            |> Graph.mapContexts (fun (p,v,l,s) -> s)
            |> Map.toList
            |> List.collect (fun (v,es) -> 
                es
                |> List.map (fun (v2,e) -> v,v2,e))
        List.fold (fun acc sL -> 
            match List.contains ((fun (a,b,c) -> (b,a,c)) sL) acc with
            |true -> acc
            |false ->sL::acc) [] sourceList
     
     
    (* Iterative *)
    ///Performs a given function on every edge of the graph.                
    let iter (action: 'Vertex -> 'Vertex -> 'Edge -> unit) (graph:Graph<'Vertex,'Label,'Edge>) : unit =
        let rec recurse g =
            match Graph.decomposeFirst g with
            | (Some c,g') -> 
                c
                |> fun (_,v,_,s) -> 
                    List.iter (fun (v',e) -> action v v' e) s
                    recurse g'
            | (None,_) -> ()
        recurse graph   
               
    ///Performs a given function on every edge of the graph, which also receives an ascending integer index.                                    
    let iteri (action: int -> 'Vertex -> 'Vertex -> 'Edge -> unit) (graph:Graph<'Vertex,'Label,'Edge>) : unit =
        let rec recurse i g =
            match Graph.decomposeFirst g with
            | (Some c,g') -> 
                c
                |> fun (_,v,_,s) -> 
                    List.iteri (fun j (v',e) -> action (j+i) v v' e) s
                    recurse (i+s.Length) g'
            | (None,_) -> ()
        recurse 0 graph

    let fold (folder : 'State -> 'Vertex -> 'Vertex -> 'Edge -> 'State) (state: 'State) (graph:Graph<'Vertex,'Label,'Edge>) : 'State =
        let rec recurse st g =
            match Graph.decomposeFirst g with
            | (Some c,g') -> 
                c
                |> fun (_,v,_,s) ->
                    List.fold (fun state (v',e) -> folder state v v' e) st s
                    |> fun st -> 
                        recurse st g'
            | (None,_) -> st
        recurse state graph

module Graph =

    ///Creates an undirected graph from a list of vertices and a list of edges
    let create vertices edges : Graph<'Vertex,'Label,'Edge> =
        Graph.empty
        |> Vertices.addMany vertices
        |> Edges.addMany edges
    
    ///Transforms a graph into a adjacency matrix of its edges. If there is no edge between two vertices, the noEdgeValue is used.
    let inline toAdjacencyMatrix (g:Graph<'Vertex,'Label,'Edge>) =
        //Create a hashmap of the vertices
        let hashMap = System.Collections.Generic.Dictionary<'Vertex,int>()
        let n = 
            let rec loop g i= 
                match Graph.decomposeFirst g with
                | Some (_,v,_,_),g -> 
                    hashMap.[v] <- i
                    loop g (i+1)        
                | None, _ -> i
            loop g 0
        //Create the matrix
        let adj : 'Edge [][] = Array.init n (fun _ -> 
            Array.zeroCreate n)
        //Fill the matrix with values by using the hashmap as an index finder
        Edges.iter (fun v1 v2 e -> 
            adj.[hashMap.Item v1].[hashMap.Item v2] <- e
            adj.[hashMap.Item v2].[hashMap.Item v1] <- e
            ) g
        adj

    ///Transfroms a graph into a adjacency matrix, maps every edge using the projection.
    let inline toAdjacencyMatrixBy (projection : 'Edge -> 'REdge) (g:Graph<'Vertex,'Label,'Edge>) =
        //Create a hashmap of the vertices
        let hashMap = System.Collections.Generic.Dictionary<'Vertex,int>()
        let n = 
            let rec loop g i= 
                match Graph.decomposeFirst g with
                | Some (_,v,_,_),g -> 
                    hashMap.[v] <- i
                    loop g (i+1)        
                | None, _ -> i+1
            loop g 0
        //Create the matrix
        let adj : 'REdge [][] = Array.init n (fun _ -> 
            Array.zeroCreate n)
        //Fill the matrix with values by using the hashmap as an index finder
        Edges.iter (fun v1 v2 e -> 
            let e' = projection e
            adj.[hashMap.Item v1].[hashMap.Item v2] <- e'
            adj.[hashMap.Item v2].[hashMap.Item v1] <- e'
            ) g
        adj