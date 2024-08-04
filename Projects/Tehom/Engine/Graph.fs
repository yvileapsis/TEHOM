namespace Tehom

open System
open System.Numerics
open Prime
open Nu

// check https://github.com/algebraic-graphs/fsharp/blob/master/Alga.FSharp/Graph.fs
// check https://github.com/CSBiology/FSharp.FGL/tree/main
// Currently using FGL, but if performance won't be satisfactory I'll implement my own

//type Graph<'Vertex, 'Edge when 'Vertex : comparison> = FSharp.FGL.Graph<'Vertex, string, 'Edge>