namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type Box = {
    Name : String
    InWorld : Boolean
}
with
    static member empty = {
        Name = String.empty
        InWorld = true
    }

    static member box name = {
        Box.empty with
            Name = name
    }

[<AutoOpen>]
module BoxInteractableDispatcher =
    type BoxInteractableDispatcher () =
        inherit Entity3dDispatcher (true, false, false)

        static member Facets = [
            typeof<RigidBodyFacet>
            typeof<StaticModelFacet>
            typeof<NavBodyFacet>
            typeof<InteractableFacet>
        ]

        static member Properties = [
            define Entity.Static false
            define Entity.BodyType Dynamic
            define Entity.Size (v3 1f 1f 1f)
            define Entity.Scale (v3 0.2f 0.2f 0.2f)
            define Entity.Presence Omnipresent
            define Entity.Static false
            define Entity.BodyType Dynamic
            define Entity.Restitution 0.5f
            define Entity.LinearDamping 0.0f
            define Entity.Substance (Density 0.1f)
            define Entity.GravityOverride None
            define Entity.StaticModel Assets.Default.StaticModel
        ]

[<AutoOpen>]
module BoxExtensions =
    type Entity with
        member this.GetBox world = this.GetModelGeneric<Box> world
        member this.SetBox value world = this.SetModelGeneric<Box> value world
        member this.Box = this.ModelGeneric<Box> ()

type BoxMessage =
    | Activate of Entity
    | ActivateInventory of Entity
    interface Message

type BoxCommand =
    | Take of Entity
    | Drop of Entity
    interface Command

type BoxDispatcher () =
    inherit Entity3dDispatcher<Box, BoxMessage, BoxCommand> (true, false, false, fun world -> Box.empty)

    static member Facets = [
        typeof<InventoryItemFacet>
    ]

    override this.Definitions (model, _) = [
//        Entity.UpdateEvent => Update
        Entity.InventoryItemDisplayName := model.Name
        Entity.ActivateEvent =|> fun evt -> Activate evt.Data
        Entity.DropEvent =|> fun evt -> ActivateInventory evt.Data
    ]

    override this.Message (model, message, entity, world) =
        match message with
        | Activate entity ->
            let model = { model with InWorld = false }
            [ Take entity ], model

        | ActivateInventory entity ->
            let model = { model with InWorld = true }
            [ Drop entity ], model

    override this.Command (model, command, entity, world) =
        match command with
        | Take entity' ->
            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.SlashSound world
            let eventTrace = EventTrace.debug "BoxDispatcher" "boxTake" "" EventTrace.empty
            let world = World.publishPlus entity entity'.TakeItemEvent eventTrace entity true false world
            just world

        | Drop entity' ->
            let position = entity'.GetPosition world
            let rotation = entity'.GetRotation world
            let interactable = entity / Simulants.Interactable
            let world = interactable.SetPosition position world
            let world = interactable.SetRotation rotation world
            just world

    override this.Content (model, entity) = [

        if model.InWorld then
            Content.entity<BoxInteractableDispatcher> Simulants.Interactable [
//                Entity.BodyPenetrationEvent => Penetration
                Entity.InteractableDisplayName := model.Name
            ]
    ]