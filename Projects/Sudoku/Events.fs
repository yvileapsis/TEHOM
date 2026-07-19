namespace Sudoku
open System
open Nu

// this module contains our user-defined events.
[<RequireQualifiedAccess>]
module Events =

    // event raised by Gameplay screen that lets the game know its time to leave gameplay
    let QuitEvent = stoa<unit> "Quit/Event"

    // event raised by Map screen when the player chooses a study node
    let MapNodeSelectedEvent = stoa<GameplayContext> "Map/NodeSelected/Event"

    // event raised by Map screen when the player wants to go back to the title screen
    let MapBackEvent = stoa<unit> "Map/Back/Event"
