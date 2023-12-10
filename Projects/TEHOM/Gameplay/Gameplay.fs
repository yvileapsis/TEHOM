namespace Tehom

open Nu
open Actor

module Gameplay =
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
        Time : int64
        State : GameplayState

        Actors: Map<ActorID, Actor>
        Compositions: Map<ActorID, Composition>
        Attachments: Set<Attachment>

        // TODO: root entities, maybe

        Player: ActorID

        Display : string

    } with
        static member makeDefault = {
            Time = 0
            State = Playing

            Actors = Map.empty
            Compositions = Map.empty
            Attachments = Set.empty

            Player = ActorID.ID "player"

            Display = "Hello world!"
        }

// this is our MMCC message type.
type GameplayMessage =
    | Update
    | SetDisplayedString of string
    | SetDisplayedStringToActorDescription of ActorID
    | Save
    | Load
    | StartQuitting
    | FinishQuitting
    interface Message

type Gameplay = Gameplay.Gameplay