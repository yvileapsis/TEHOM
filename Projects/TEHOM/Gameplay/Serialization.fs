namespace Tehom

open System.IO
open Prime

open Nu
open Actor
open Trait
open Ability
open Gameplay
open FSharp.Configuration

module Serialization =

    type YamlStructure = YamlConfig<"Assets/Actors/default.yaml">

    type SavedActors = {

        Descriptions: Map<ActorID, DescriptionTrait>
        Volumes: Map<ActorID, VolumeTrait>
        Masses: Map<ActorID, MassTrait>
        Health: Map<ActorID, HealthTrait>
        Abilities: Map<ActorID, Set<Ability>>
        Composed: Map<ActorID, Composition>
        Attached: Set<Attachment>

    } with
        static member makeDefault = {
            Descriptions = Map.empty
            Volumes = Map.empty
            Masses = Map.empty
            Health = Map.empty
            Abilities = Map.empty
            Composed = Map.empty
            Attached = Set.empty
        }

        member this.saveToFile file =
            let fieldSymbol = valueToSymbol this
            let fileStr = PrettyPrinter.prettyPrintSymbol fieldSymbol PrettyPrinter.defaultPrinter
            try File.WriteAllText (file, fileStr) with _ -> ()

        static member loadFromFile file =
            let text = try File.ReadAllText file with _ -> ""
            scvalue<SavedActors> text

        member this.concat save = {
            Descriptions = Map.concat save.Descriptions this.Descriptions
            Volumes = Map.concat save.Volumes this.Volumes
            Masses = Map.concat save.Masses this.Masses
            Health = Map.concat save.Health this.Health
            Abilities = Map.concat save.Abilities this.Abilities
            Composed = Map.concat save.Composed this.Composed
            Attached = Set.union save.Attached this.Attached
        }

    let fromYaml =
        let data = YamlStructure()
        data.Load(Assets.Actors.Directory + "\\" + Assets.Actors.ActorsContent)

        let mapActorFromYaml oldMap (yamlActor: YamlStructure.actors_Item_Type) =

            let description =
                match yamlActor.name, yamlActor.description with
                | "", "" -> None
                | name, description -> Some {
                    Name = GeneratableString.String name
                    Description = GeneratableString.String description
                }

            let abilities =
                let stringToAbility set (ability: YamlStructure.actors_Item_Type.abilities_Item_Type) =
                    // TODO: all types
                    match ability.ability with
                    | _ -> set

                Seq.fold stringToAbility Set.empty yamlActor.abilities

            let actor = {
                Actor.makeDefault with
                    Description = description;
                    Abilities = abilities
            }

            let addActorToMap map actorID =
                Map.add (ActorID.ID actorID) actor map

            Seq.fold addActorToMap oldMap yamlActor.guid

        let mapActorComposedFromYaml oldMap (yamlActor: YamlStructure.actors_Item_Type) =

            let parent = ActorID.ID yamlActor.parent

            let children =
                if Map.containsKey parent oldMap
                then oldMap[parent]
                else Map.empty

            let addChildToChildren children child =
                Map.add (ActorID.ID child) Actor.Simple children

            let children = Seq.fold addChildToChildren children yamlActor.guid

            Map.add parent children oldMap
        ()
//            { this with
//                Actors = Seq.fold mapActorFromYaml this.Actors data.actors
//                ActorsComposed = Seq.fold mapActorComposedFromYaml this.ActorsComposed data.actors
//            }

    let fromGameplay gameplay =
        let mapOfRecordsToRecordOfMaps saved =

            let description key (value: Actor) map =
                match value.Description with
                | Some x -> Map.add key x map
                | None -> map

            let volumes key (value: Actor) map =
                match value.Volume with
                | Some x -> Map.add key x map
                | None -> map

            let health key (value: Actor) map =
                match value.Health with
                | Some x -> Map.add key x map
                | None -> map

            let masses key (value: Actor) map =
                match value.Mass with
                | Some x -> Map.add key x map
                | None -> map

            let abilities key (value: Actor) map =
                match value.Abilities with
                | e when (Set.isEmpty e) -> map
                | x -> Map.add key x map


            gameplay.Actors
            |> Map.fold
                (fun acc key value -> {
                    acc with
                        Descriptions = description key value acc.Descriptions
                        Volumes = volumes key value acc.Volumes
                        Masses = masses key value acc.Masses
                        Abilities = abilities key value acc.Abilities
                        Health = health key value acc.Health
                })
                saved


        {
            (mapOfRecordsToRecordOfMaps SavedActors.makeDefault)
            with
                Composed = gameplay.Compositions
                Attached = gameplay.Attachments
        }
    let toGameplay this gameplay =
        let allKeys =
            Set.ofSeq (Seq.concat [
                Map.keys this.Descriptions
                Map.keys this.Health
                Map.keys this.Masses
                Map.keys this.Volumes
                Map.keys this.Abilities
            ])

        let actor key = {
            Description = Map.tryFind key this.Descriptions
            Health = Map.tryFind key this.Health
            Mass = Map.tryFind key this.Masses
            Volume = Map.tryFind key this.Volumes
            Abilities =
                match Map.tryFind key this.Abilities with
                | Some x -> x
                | None -> Set.empty
        }

        let actors = Set.fold (fun map key -> Map.add key (actor key) map) Map.empty allKeys

        {
        gameplay with
            Actors = actors
            Compositions = this.Composed
            Attachments = this.Attached
        }

    let saveToFile gameplay =
        (fromGameplay gameplay).saveToFile Assets.Actors.ActorsContentTehom

    let loadFromFile gameplay =
        let old = fromGameplay gameplay

        let newer =
            Directory.EnumerateFiles (Assets.Actors.ActorsContent, "*.tehom")
            |> Seq.fold (fun (saved: SavedActors) string ->
                saved.concat (SavedActors.loadFromFile string)) old

        toGameplay (old.concat newer) gameplay