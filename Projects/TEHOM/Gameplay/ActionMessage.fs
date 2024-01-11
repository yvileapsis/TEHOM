namespace Tehom

open Nu
open Ability
open Actions
open Actor
open Trait
open TehomID
open Action

module ActionMessage =

    let action (gameplay: Gameplay) actorID : string =
//                    $"{Actions.canSense actorID gameplay.Compositions gameplay.Actors}"
//                match Map.tryFind actorID gameplay.Actors with
//                | Some actor -> getLookActions
//                | _ -> $"did not find %A{actorID}, weird!"
//        try
            match Map.containsKey actorID gameplay.Actors with
            | true ->
                match Map.containsKey actorID gameplay.Compositions with
                | true ->
                    let choices = choices actorID gameplay.Actors gameplay.Compositions gameplay.Abilities gameplay.Actions
                    let choices2 = tempFilter choices
                    match choices2 with
                    | Ok x -> $"success %i{Set.count x} out of %i{Set.count choices}"
                    | Error x -> $"%A{x}"
                | false -> $"did not find %A{actorID} composition!"
            | false -> $"did not find %A{actorID} actor!"

//        with
 //           A -> $"%A{A}"