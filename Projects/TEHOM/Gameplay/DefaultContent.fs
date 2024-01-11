namespace Tehom

open Nu
open Ability
open Actions
open Actor
open Trait
open TehomID
open Action

module DefaultContent =

    let cat (gameplay: Gameplay) =

        let ``id think/animal`` = AbilityID (ID "think\\animal")
        let ``action think/animal`` = {
            Ability.default' with
                AbilityGroup = AbilityGroupID (ID "think")
                Initiative = Gain 2
                Focus = Gain 4
                Levels = Set.ofList [
                    0u, 0u, 1.0
                ]
            }

        let ``id see/cat`` = AbilityID (ID "see\\cat")
        let ``action see/cat`` = {
            Ability.default' with
                AbilityGroup = AbilityGroupID (ID "see")
                Initiative = Cost 1
                Focus = Cost 1
                Levels = Set.ofList [
                    2u, 2u, 2.0   // look at siblings' children
                    2u, 1u, 1.0   // look at siblings
                    2u, 0u, 1.0   // look at parent
                    1u, 0u, 0.0   // look at self
                    1u, 1u, 0.0   // look at children
                    1u, 2u, 1.0   // look at grandchildren
                    1u, 3u, 2.0   // look at great grandchildren
                ]
            }

        let actions actions =
            actions
            |> Map.toList
            |> List.append [
                ``id think/animal``, ``action think/animal``
                ``id see/cat``, ``action see/cat``
            ]
            |> Map.ofList


        let ``id cat/brain`` = ID "cat\\brain"
        let ``actor cat/brain`` = { Actor.default' with Abilities = Set.ofList [ ``id think/animal`` ] }

        let ``id cat/ears`` = ID "cat\\ears"
        let ``actor cat/ears`` = { Actor.default' with Abilities = Set.ofList [ AbilityID (ID "hear\\cat") ] }

        let ``id cat/eyes`` = ID "cat\\eyes"
        let ``actor cat/eyes`` = { Actor.default' with Abilities = Set.ofList [ ``id see/cat`` ] }

        let ``id cat/legBackRight`` = ID "cat\\legBackRight"
        let ``id cat/legBackLeft`` = ID "cat\\legBackLeft"

        let ``actor cat/legBack`` = { Actor.default' with Abilities = Set.ofList [ AbilityID (ID "walk\\cat") ] }
        let ``id cat/legFrontLeft`` = ID "cat\\legFrontLeft"

        let ``id cat/legFrontRight`` = ID "cat\\legFrontRight"
        let ``actor cat/legFront`` = {
            Actor.default' with
                Abilities = Set.ofList [
                    AbilityID (ID "walk\\cat")
                    AbilityID (ID "attack\\cat\\paw")
                ]
        }

        let ``id cat/mouth`` = ID "cat\\mouth"
        let ``actor cat/mouth`` = {
            Actor.default' with
                Abilities = Set.ofList [
                    AbilityID (ID "eat\\cat")
                    AbilityID (ID "grab\\cat\\mouth")
                    AbilityID (ID "attack\\cat\\mouth")
                ]
        }

        let ``id cat/skin`` = ID "cat\\skin"
        let ``id cat/skull`` = ID "cat\\skull"

        let ``id cat/tail`` = ID "cat\\tail"
        let ``actor cat/tail`` = { Actor.default' with Abilities = Set.ofList [ AbilityID (ID "balance\\cat") ] }

        let ``id cat/torso`` = ID "cat\\torso"
        let ``actor cat/torso`` = { Actor.default' with Abilities = Set.ofList [ AbilityID (ID "balance\\cat") ] }

        let ``id cat`` = ID "cat"
        let ``actor cat`` : Actor = Actor.default'
        let ``description cat`` : DescriptionTrait = {
            Name = String "Cat the NPC"
            Description = String "Simple NPC for testing NPC creation."
        }

        let ``composition cat`` : Composition = Composition (
            Map.ofList [
                ActorID ``id cat/brain``, Controls 100
                ActorID ``id cat/ears``, Controls 100
                ActorID ``id cat/eyes``, Controls 100
                ActorID ``id cat/legBackRight``, Controls 100
                ActorID ``id cat/legBackLeft``, Controls 100
                ActorID ``id cat/legFrontLeft``, Controls 100
                ActorID ``id cat/legFrontRight``, Controls 100
                ActorID ``id cat/mouth``, Controls 100
                ActorID ``id cat/skin``, Controls 100
                ActorID ``id cat/skull``, Controls 100
                ActorID ``id cat/tail``, Controls 100
                ActorID ``id cat/torso``, Controls 100
            ]
        )

        let descriptions =
            Map.add ``id cat`` ``description cat``

        let actors =
            Map.toList
            >> List.append [
                ActorID ``id cat``, ``actor cat``
                ActorID ``id cat/brain``, ``actor cat/brain``
                ActorID ``id cat/ears``, ``actor cat/ears``
                ActorID ``id cat/eyes``, ``actor cat/eyes``
                ActorID ``id cat/legBackRight``, ``actor cat/legBack``
                ActorID ``id cat/legBackLeft``, ``actor cat/legBack``
                ActorID ``id cat/legFrontLeft``, ``actor cat/legFront``
                ActorID ``id cat/legFrontRight``, ``actor cat/legFront``
                ActorID ``id cat/mouth``, ``actor cat/mouth``
                ActorID ``id cat/tail``, ``actor cat/tail``
                ActorID ``id cat/torso``, ``actor cat/torso``
            ]
            >> Map.ofList

        let composition =
            Map.add (ActorID ``id cat``) ``composition cat``

        { gameplay with
            Descriptions = descriptions gameplay.Descriptions
            Abilities = actions gameplay.Abilities
            Actors = actors gameplay.Actors
            Compositions = composition gameplay.Compositions
        }


    let table (gameplay: Gameplay) =

        let ``id table`` = ID "table"
        let ``actor table`` = Actor.default'
        let ``description table`` = {
            Name = String "Table McTableface"
            Description = String "A normal wooden table for testing non-alive entities."
        }
        let ``composition table`` = Composition (
            Map.ofList [
                ActorID (ID "table\\surface"), Simple
                ActorID (ID "table\\leg1"), Simple
                ActorID (ID "table\\leg2"), Simple
                ActorID (ID "table\\leg3"), Simple
                ActorID (ID "table\\leg4"), Simple
            ]
        )

        let descriptions =
            Map.add ``id table`` ``description table``

        let actors =
            Map.add (ActorID ``id table``) ``actor table``

        let composition =
            Map.add (ActorID ``id table``) ``composition table``

        { gameplay with
            Descriptions = descriptions gameplay.Descriptions
            Actors = actors gameplay.Actors
            Compositions = composition gameplay.Compositions
        }


    let world (gameplay: Gameplay) =

        let ``id think`` = ActionID (ID "think")
        let ``action think`` = {
            Action.default' with
                Order = [
                    AbilityGroupID (ID "think")
                ]
        }

        let ``id look`` = ActionID (ID "look")
        let ``action look`` = {
            Action.default' with
                Order = [
                    AbilityGroupID (ID "think")
                    AbilityGroupID (ID "see")
                ]
        }

        let actions =
            Map.add ``id look`` ``action look``
            >> Map.add ``id think`` ``action think``

        let ``id key`` = ID "key"
        let ``actor key`` = Actor.default'
        let ``description key`` = {
            Name = String "Shiny key"
            Description = String "A key that shines and opens doors."
        }

        let ``id dungeon/north`` = ID "dungeon\\north"
        let ``actor dungeon/north`` = Actor.default'
        let ``description dungeon/north`` = {
            Name = String "Northern Room"
            Description = String "The room up north, dark and cold."
        }
        let ``composition dungeon/north`` = Composition (
            Map.ofList [
                ActorID (ID "cat"), Simple
                ActorID ``id key``, Simple
            ]
        )

        let ``id dungeon/south`` = ID "dungeon\\south"
        let ``actor dungeon/south`` = Actor.default'
        let ``description dungeon/south`` = {
            Name = String "Southern Room"
            Description = String "The room down south, lit and warm."
        }
        let ``composition dungeon/south`` = Composition (
            Map.ofSeq [
                ActorID (ID "table"), Simple
            ]
        )

        let ``id dungeon`` = ID "dungeon"
        let ``composition dungeon`` : Composition = Composition (
            Map.ofSeq [
                ActorID ``id dungeon/north``, Simple
                ActorID ``id dungeon/south``, Simple
            ]
        )

        let ``id tehom`` = ID "tehom"
        let ``actor tehom`` = Actor.default'
        let ``description tehom`` = {
            Name = String "Tehom"
            Description = String "The world, needlessly cruel."
        }
        let ``composition tehom`` = Composition (
            Map.ofSeq [
                ActorID ``id dungeon``, Simple
            ]
        )

        let descriptions =
            Map.add ``id key`` ``description key``
            >> Map.add ``id dungeon/north`` ``description dungeon/north``
            >> Map.add ``id dungeon/south`` ``description dungeon/south``
            >> Map.add ``id tehom`` ``description tehom``

        let actors =
            Map.add (ActorID ``id key``) ``actor key``
            >> Map.add (ActorID ``id dungeon/north``) ``actor dungeon/north``
            >> Map.add (ActorID ``id dungeon/south``) ``actor dungeon/south``
            >> Map.add (ActorID ``id tehom``) ``actor tehom``

        let composition =
            Map.add (ActorID ``id dungeon``) ``composition dungeon``
            >> Map.add (ActorID ``id dungeon/north``) ``composition dungeon/north``
            >> Map.add (ActorID ``id dungeon/south``) ``composition dungeon/south``
            >> Map.add (ActorID ``id tehom``) ``composition tehom``

        { gameplay with
            Descriptions = descriptions gameplay.Descriptions
            Actors = actors gameplay.Actors
            Compositions = composition gameplay.Compositions
            Actions = actions gameplay.Actions
        }

(*

    Entities can be looked from two angles, structural composition, of what consists of what:
    Table is composed of: table top, 4 table legs.

    And physical composition:
    * Table top
    * 4 table legs, each attached to table top



    Alive entities are a little more complex.

    What is a cat?

    Alive entities can be also looked at from two angles:

    Physical composition:
    * torso
    * 2 front legs, 2 back legs attached to torso
    * tail attached to torso
    * head attached to torso
        head of course can also be split:
        * skull
        * two eyes attached to skull
        * brain attached to skull
        * skin attached to skull

    Another way to look at the cat is through systems which correspond to its abilities,
    which are retrieved through filtering structural composition for its abilities:
    Cat has:
    * Nervous system -- Filter by CanThink -> brain
    * Sensory system -- Filter by CanSense -> Eyes, nose, ears, skin, etc
    * Locomotion system -- Filter by CanMove -> Legs, paws, tail (?), ???
    * Manipulation system -- Filter by CanGrab -> paws, mouth
    * Talking system -- Filter by CanTalk -> mouth, lungs (?)
    * Attack system - Filter by IsWeapon -> paws, mouth
    New systems can be added later as needed, as it's literally just a filter over the composed entity.

*)

    let defaultContent =
        cat >> table >> world