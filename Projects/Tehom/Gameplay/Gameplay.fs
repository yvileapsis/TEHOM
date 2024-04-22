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

    State : GameplayState
    GameTime : int64
    Story : Story
    CurrentAct : int

} with
    static member empty = {
        State = Playing
        GameTime = 0
        Story = Story.empty
        CurrentAct = 0
    }

    static member initial = {
        Gameplay.empty with
            Story = Story.initial
    }

// this is our MMCC message type.
type GameplayMessage =
    | Update
    | InputString of TehomID
    | Action of TehomID * TehomChoice
    | StartQuitting
    | FinishQuitting
    interface Message