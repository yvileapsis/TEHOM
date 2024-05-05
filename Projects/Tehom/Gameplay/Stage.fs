namespace Tehom

open System
open Nu
open FSharp.FGL

open Abyss

// gameworld + time + quest stages
type Stage = {
    Abyss : Abyss
    Time : TehomTime
}
with
    static member empty = {
        Abyss = Abyss.empty
        Time = TehomTime.empty
    }

    static member initial = { Stage.empty with Abyss = Abyss.initial }