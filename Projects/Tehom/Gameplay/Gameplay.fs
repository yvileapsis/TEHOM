namespace Tehom

open Nu
open TehomID

// this represents that state of the simulation during gameplay.
type GameplayState =
    | Playing
    | Quitting
    | Quit

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
// if you wish to use clock time instead (https://github.com/bryanedds/Nu/wiki/GameTime-and-its-Polymorphic-Nature),
// you could use `Time : single` instead.
type Gameplay = {

    GameTime : int64

    Time : TehomTime

    State : GameplayState

    Actors : TehomActors

    Player: TehomID

    Display : string

} with
    static member empty = {
        GameTime = 0
        State = Playing
        Time = TehomTime.empty
        Actors = TehomActors.empty
        Player = TehomID.empty
        Display = ""
    }

    static member initial = {
        Gameplay.empty with
            Player = ID "player"
            Actors = TehomActors.addDefault Gameplay.empty.Actors
            Display = "Hello world!\nOn two lines!"
    }

// this is our MMCC message type.
type GameplayMessage =
    | Update
    | InputString of TehomID
    | Action of TehomID * TehomChoice
    | Save
    | Load
    | StartQuitting
    | FinishQuitting
    interface Message