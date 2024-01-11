namespace Tehom

open Nu
open Actor
open TehomID
open Trait
open Ability
open Action

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


        Descriptions: Map<TehomID, DescriptionTrait>

        Actors: Map<ActorID, Actor>
        Compositions: Map<ActorID, Composition>

        Abilities: Map<AbilityID, Ability>

        Actions: Map<ActionID, Action>

        Attachments: Set<Attachment>

        // TODO: root entities, maybe

        Player: TehomID

        Display : string

    } with
        static member default' = {
            Time = 0
            State = Playing

            Descriptions = Map.empty

            Actors = Map.empty
            Compositions = Map.empty

            Abilities = Map.empty
            Actions = Map.empty

            Attachments = Set.empty

            Player = ID "player"

            Display = "Hello world!"
        }

// this is our MMCC message type.
type GameplayMessage =
    | Update
    | SetDisplayedString of string
    | DoAction of TehomID
    | Save
    | Load
    | StartQuitting
    | FinishQuitting
    interface Message

type Gameplay = Gameplay.Gameplay