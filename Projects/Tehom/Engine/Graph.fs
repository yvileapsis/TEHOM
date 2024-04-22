namespace Tehom

open System
open System.Numerics
open Prime
open Nu

module Graph =
    // TODO: check https://github.com/algebraic-graphs/fsharp/blob/master/Alga.FSharp/Graph.fs
    // TODO: check https://github.com/CSBiology/FSharp.FGL/tree/main

    type Graph<'Vertex, 'Edge> = Graph of 'Vertex * 'Edge