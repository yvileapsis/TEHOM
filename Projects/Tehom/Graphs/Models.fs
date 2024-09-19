namespace FGL.Algorithm

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

open FGL

///Contains generic functions for creating model graphs
module Models =
    
    let gilbert 
        (newGraph: 'Graph) (fVertexKey: int -> 'Vertex) (fAddVertex: 'Vertex -> 'Graph -> 'Graph) (fAddEdge: 'Vertex -> 'Vertex -> 'Graph -> 'Graph) 
        (vertexCount: int) (p: float) = 
            let rnd = new System.Random()
            let rec addVertices i g =
                if i = vertexCount then 
                    g
                else 
                    addVertices (i+1) (fAddVertex (fVertexKey i) g)
            let rec innerAddEdges i j g =
                if j = vertexCount then g
                else 
                    if rnd.NextDouble() > p then
                        innerAddEdges i (j+1) g
                    elif i = j then
                        innerAddEdges i (j+1) g
                    else 
                        innerAddEdges i (j+1) (fAddEdge (fVertexKey i) (fVertexKey j) g)
            let rec addEdges i g = 
                if i = vertexCount then g
                else addEdges (i+1) (innerAddEdges i 0 g)
            newGraph
            |> addVertices 0
            |> addEdges 0

    let erdosRenyi 
        (newGraph: 'Graph) (fVertexKey: int -> 'Vertex) (fAddVertex: 'Vertex -> 'Graph -> 'Graph) (fAddEdge: 'Vertex -> 'Vertex -> 'Graph -> 'Graph) 
        (vertexCount: int) (edgeCount: int) = 
            let rnd = new System.Random()
            let rec addVertices i g =
                if i = vertexCount then 
                    g
                else 
                    addVertices (i+1) (fAddVertex (fVertexKey i) g)
            let rec forceAddEdge g (s: Set<int*int>) =
                let r = rnd.Next(0,vertexCount),rnd.Next(0,vertexCount)
                if s.Contains r then
                    forceAddEdge g (s: Set<int*int>)
                else 
                    fAddEdge (fVertexKey (fst r)) (fVertexKey (snd r)) g, Set.add r s
            let rec addEdges i (g,(s: Set<int*int>)) = 
                if i = edgeCount then g
                else 
                    addEdges (i+1) (forceAddEdge g s)
            newGraph
            |> addVertices 0
            |> fun g -> addEdges 0 (g,Set.empty)
    
//    let barabasiAlbert 
//       (newGraph: 'Graph) (fVertexKey: int -> 'Vertex) (fAddVertex: 'Vertex -> 'Graph -> 'Graph) (fAddEdge: 'Vertex -> 'Vertex -> 'Graph -> 'Graph)
//       (fGetDegrees: 'Graph -> seq<int*int>) (vertexCount: int) (initialVertexCount: int) (edgesPerIteration: int) =
//            let addVertexWithEdges