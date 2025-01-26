namespace MyGame
open System
open Nu

type CursorType =
    | CursorNormal
    | CursorUp
    | CursorDown
    | CursorLeft
    | CursorRight

[<RequireQualifiedAccess>]
module Events =

    let AttackEvent = stoa<Entity> "Attack/Event"
    let DieEvent = stoa<Entity> "Die/Event"
    let QuitEvent = stoa<unit> "Quit/Event"
    let SelectionEvent = stoa<Entity list> "Select/Event"
    let BeginSelectingEvent = stoa<Entity list> "BeginSelect/Event"
    let SetCursorEvent = stoa<(CursorType * int) list> "SetCursor/Event"