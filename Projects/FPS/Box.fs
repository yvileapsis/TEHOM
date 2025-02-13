namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type Box = {
    Name : String
    Hovered : Boolean
}
with
    static member empty = {
        Name = "Box McBoxface"
        Hovered = false
    }

type BoxMessage =
    | Update
    | UpdateInputKey of KeyboardKeyData
    interface Message

type BoxCommand =
    | Penetration
    | TakeItem
    interface Command

type BoxDispatcher () =
    inherit Entity3dDispatcher<Box, BoxMessage, BoxCommand> (true, false, false, fun world -> Box.empty)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>]

    override this.Definitions (_, _) =
        [Entity.Size == v3 1f 1f 1f
         Entity.Scale == v3 0.2f 0.2f 0.2f
         Entity.Presence == Omnipresent
         Entity.Static == false
         Entity.BodyType == Dynamic
         Entity.Restitution == 0.5f
         Entity.LinearDamping == 0.0f
         Entity.Substance == Density 0.1f
         Entity.GravityOverride == None
         Entity.Observable == true
         Entity.StaticModel == Assets.Default.StaticModel
         Entity.UpdateEvent => Update
         Entity.BodyPenetrationEvent => Penetration
         Game.KeyboardKeyDownEvent =|> fun evt -> UpdateInputKey evt.Data]

    override this.Message (model, message, entity, world) =
        match message with
        | Update ->
            //let localTime = world.UpdateTime - startTime
            //let world = if localTime = BulletLifeTime then World.destroyEntity entity world else world
            let screenCenterRay =
                let eyeCenter = World.getEye3dCenter world
                let eyeRotation = World.getEye3dRotation world
                let eyeFieldOfView = World.getEye3dFieldOfView world
                let mousePosition = World.getMousePosition world
                Viewport.mouseToWorld3d eyeCenter eyeRotation eyeFieldOfView mousePosition world.RasterViewport

            let intersections =
                let ray = screenCenterRay
                let origin = ray.Origin
                let finale = ray.Origin + 100f * ray.Direction
                let segment = segment3 origin finale
                let array = World.rayCast3dBodies segment 0xFFFFFFFF false world
                let array =
                    array
                    |> Array.choose (fun x ->
                        match x.BodyShapeIntersected.BodyId.BodySource with
                        | :? Entity as intersected when intersected.Is<BoxDispatcher> world ->
                            Some intersected
                        | _ ->
                            None
                    )
                array

            let hovered =
                match Array.tryHead intersections with
                | Some intersectionData when intersectionData = entity ->
                    true
                | _ ->
                    false

            let model = { model with Hovered = hovered }

            just model

        | UpdateInputKey keyboardKeyData ->
            [
                if keyboardKeyData.KeyboardKey = KeyboardKey.E && model.Hovered then TakeItem
            ], model

    override this.Command (model, command, entity, world) =
        match command with
        | Penetration ->
            //let world = World.destroyEntity entity world
            just world

        | TakeItem ->

            let player = Simulants.GameplayPlayer

            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.SlashSound world
            let world = if model.Hovered then World.destroyEntity entity world else world

            let eventTrace = EventTrace.debug "BoxDispatcher" "boxTake" "" EventTrace.empty
            let world = World.publishPlus entity player.TakeItemEvent eventTrace entity true false world

            just world

    override this.Content (model, _) = [
        if model.Hovered then
            Content.text "Name" [
                Entity.Absolute == true
                Entity.PositionLocal == v3 0f -120.0f 0.0f
                Entity.Size == v3 128.0f 32.0f 0.0f
                Entity.Text := $"{model.Name}"
                Entity.FontSizing == Some 10
                Entity.MountOpt == None
            ]

            Content.text "Take" [
                Entity.Absolute == true
                Entity.PositionLocal == v3 0f -130.0f 0.0f
                Entity.Size == v3 128.0f 32.0f 0.0f
                Entity.Text := $"E) Take"
                Entity.FontSizing == Some 10
                Entity.MountOpt == None
            ]

        if model.Hovered then
            Content.staticSprite "CrosshairPrompt" [
                Entity.Absolute == true
                Entity.Size == v3 16f 16f 0f
                Entity.StaticImage == Assets.Gui.CrosshairPrompt
                Entity.MountOpt == None
            ]
    ]