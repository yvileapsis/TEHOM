namespace Tehom

open System
open Prime
open Nu

open Actor
open Trait
open FSharp.Configuration

type ActorsData = YamlConfig<"Assets\\Actors\\default.yaml">

[<AutoOpen>]
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
        ActorsComposed: Map<ActorID, Set<ActorID> * Composed>
        ActorsAttached: List<Set<ActorID> * Attached>

        // TODO: root entities, maybe

        Player: ActorID

        Display : string

    } with
        static member makeDefault = {
            Time = 0
            State = Playing

            Actors = Map.empty
            ActorsComposed = Map.empty
            ActorsAttached = List.empty

            Player = ActorID.ID "player"

            Display = "Hello world!"
        }

        member model.serializeYaml =
            let data = ActorsData()
            data.Load(Assets.Actors.Directory + "\\" + Assets.Actors.ActorsContent)

            let mapActorFromYaml oldMap (yamlActor: ActorsData.actors_Item_Type) =
                let actorID = ActorID.ID yamlActor.guid

                let actor = {
                    Actor.makeDefault
                    with Description = Some {
                        Name = GeneratableString.String yamlActor.name
                        Description = GeneratableString.String yamlActor.description
                    }
                }

                Map.add actorID actor oldMap

            let mapActorComposedFromYaml oldMap (yamlActor: ActorsData.actors_Item_Type) =
                let actorID = ActorID.ID yamlActor.guid

                let actorComposed = Set.ofSeq (Seq.map ActorID.ID yamlActor.composed), Cohesion yamlActor.composedType

                Map.add actorID actorComposed oldMap


            { model with
                Actors = Seq.fold mapActorFromYaml model.Actors data.actors
                ActorsComposed = Seq.fold mapActorComposedFromYaml model.ActorsComposed data.actors
            }

type Gameplay = Gameplay.Gameplay